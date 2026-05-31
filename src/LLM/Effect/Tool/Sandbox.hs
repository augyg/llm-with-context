{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- | Sandbox backends for the 'Tool' effect's command execution.
--
-- @bwrap@ (the jailing runGhcBWrap uses) is Linux-only, so a single sandbox
-- can't cover Mac/Windows. Instead the confinement mechanism is a swappable
-- 'SandboxBackend':
--
--   * 'BubblewrapBackend' — Linux kernel-namespace isolation via @bwrap@
--     (bind the root at @\/project@, ro-bind the toolchain store, @--dev@\/
--     @--proc@\/@--die-with-parent@\/@--chdir@). This mirrors
--     @RunGhcBWrap.runSandboxedExecutable@.
--   * 'SandboxExecBackend' — macOS Seatbelt via @sandbox-exec -p \<profile\>@,
--     confining writes to the root.
--   * 'NoSandboxBackend' — portable fallback (Windows / anywhere with neither
--     tool): no OS isolation, only the best-effort path 'jail' below. This is
--     NOT real isolation and says so.
--
-- 'bwrap' confines only child processes, so (exactly as runGhcBWrap does) only
-- 'runCommand' is wrapped; the filesystem operations run host-side, confined to
-- the jailed root. The argv/profile builders are pure so they can be unit
-- tested without the binaries present.
--
-- VERIFIED-BY-CONSTRUCTION ONLY: the pure builders + 'jail' are tested, but the
-- actual spawn has not run on a real Linux/macOS host from here (neither
-- @bwrap@ nor @sandbox-exec@ is installed in this environment).
module LLM.Effect.Tool.Sandbox
  ( -- * Backend selection
    SandboxBackend (..)
  , BWrapConfig (..)
  , SbExecConfig (..)
  , defaultBWrapConfig
  , defaultSbExecConfig
  , detectBackend
    -- * Path jail
  , jail
  , shellCommandPaths
    -- * Pure command builders (unit-testable without the binaries)
  , bubblewrapArgv
  , sandboxExecProfile
  , sandboxExecArgv
    -- * Confined execution
  , execSandboxed
    -- * Sandboxed Tool interpreter
  , SandboxConfig (..)
  , mkSandboxConfig
  , defaultMaxOutputBytes
  , runToolSandboxed
  ) where

import Prelude hiding (appendFile, readFile, writeFile)

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified System.Directory as Dir
import System.Environment (lookupEnv)
import System.FilePath (isAbsolute, splitDirectories, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (cwd, env), proc, readCreateProcessWithExitCode)
#if defined(STATIC_SANDBOX_EXE)
-- Only imported when the staticWhich splices below are compiled in; otherwise
-- it would be an unused import (and -Werror=unused-imports is on).
import System.Which (staticWhich)
#endif

import Effectful (Eff, IOE, (:>))

import LLM.Effect.Tool
  ( ShellCommand (..)
  , RunOptions (..)
  , Tool
  , ToolHandlers (..)
  , ToolResult (..)
  , exitCodeToInt
  , globIO
  , renderShellCommand
  , runToolWith
  , statIO
  , tryIO
  , writeFileMkdir
  )

-- Backend selection ----------------------------------------------------------

-- | Linux bubblewrap backend.
data BWrapConfig = BWrapConfig
  { _bwrap_exe :: FilePath
    -- ^ Path to the @bwrap@ executable (default @"bwrap"@, resolved on PATH).
  , _bwrap_readOnlyBinds :: [FilePath]
    -- ^ Host directories ro-bound at the same path inside the sandbox so the
    -- command's binaries + shared libraries resolve. On nix this is just
    -- @["\/nix\/store"]@; on a generic distro use e.g.
    -- @["\/usr","\/bin","\/lib","\/lib64","\/etc"]@.
  , _bwrap_projectMount :: FilePath
    -- ^ Where the sandbox root is bound inside the namespace (default
    -- @"\/project"@); the command runs with this as its working directory.
  } deriving (Show)

-- | macOS @sandbox-exec@ (Seatbelt) backend.
data SbExecConfig = SbExecConfig
  { _sbExec_exe :: FilePath
    -- ^ Path to @sandbox-exec@ (default @"sandbox-exec"@).
  , _sbExec_allowNetwork :: Bool
    -- ^ Whether the generated profile permits outbound network.
  } deriving (Show)

-- | How 'runCommand' is confined. Verbose constructors per the no-shortforms
-- rule.
data SandboxBackend
  = BubblewrapBackend BWrapConfig
  | SandboxExecBackend SbExecConfig
  | NoSandboxBackend
  deriving (Show)

defaultBWrapConfig :: BWrapConfig
defaultBWrapConfig = BWrapConfig
  { _bwrap_exe = bwrapExe
  , _bwrap_readOnlyBinds = ["/nix/store"]
  , _bwrap_projectMount = "/project"
  }

defaultSbExecConfig :: SbExecConfig
defaultSbExecConfig = SbExecConfig
  { _sbExec_exe = sandboxExecExe
  , _sbExec_allowNetwork = False
  }

-- | Path to the @bwrap@ executable. With the @static-sandbox-exe@ cabal flag
-- (the default) on Linux it is resolved at COMPILE time via 'staticWhich' and
-- baked in as an absolute @\/nix\/store@ path — which is both how it's found AND
-- how nix retains bubblewrap as a runtime dependency (the baked path is a store
-- reference). With the flag off (or off Linux) it falls back to a bare name
-- resolved on PATH at runtime, so the build needs no @bwrap@ present.
bwrapExe :: FilePath
#if defined(STATIC_SANDBOX_EXE) && defined(linux_HOST_OS)
bwrapExe = $(staticWhich "bwrap")
#else
bwrapExe = "bwrap"
#endif

-- | Path to @sandbox-exec@. On macOS it is baked at compile time via
-- 'staticWhich'; elsewhere it falls back to the fixed system location (it is a
-- macOS system binary, not a nixpkgs package).
sandboxExecExe :: FilePath
#if defined(STATIC_SANDBOX_EXE) && defined(darwin_HOST_OS)
sandboxExecExe = $(staticWhich "sandbox-exec")
#else
sandboxExecExe = "/usr/bin/sandbox-exec"
#endif

-- | The backend for the current build platform, with its executable path baked
-- in at compile time ('bwrapExe' \/ 'sandboxExecExe'). Linux → bubblewrap,
-- macOS → sandbox-exec, otherwise the portable no-isolation fallback.
detectBackend :: IO SandboxBackend
detectBackend = pure
#if defined(linux_HOST_OS)
  (BubblewrapBackend defaultBWrapConfig)
#elif defined(darwin_HOST_OS)
  (SandboxExecBackend defaultSbExecConfig)
#else
  NoSandboxBackend
#endif

-- Path jail ------------------------------------------------------------------

-- | Confine a requested path under @root@: reject absolute paths and any @..@
-- traversal; otherwise return @root '</>' p@. Best-effort (not a chroot) — the
-- OS backends ('BubblewrapBackend' \/ 'SandboxExecBackend') are what actually
-- enforce isolation; this just keeps relative paths from escaping.
jail :: FilePath -> FilePath -> Either T.Text FilePath
jail root p
  | isAbsolute p = Left "absolute paths are not allowed in the sandbox"
  | ".." `elem` splitDirectories p = Left "path traversal ('..') is not allowed in the sandbox"
  | otherwise = Right (root </> p)

-- | The filesystem path(s) a command reads/operates on — jailed before exec.
shellCommandPaths :: ShellCommand -> [FilePath]
shellCommandPaths = \case
  Grep _ path _ _ -> [path]
  Ls path _ _ -> [path]
  Find root _ -> [root]
  Head path _ -> [path]
  Tail path _ -> [path]
  Wc path -> [path]

-- Pure command builders ------------------------------------------------------

-- | Build the @bwrap@ argv (everything after the @bwrap@ executable) to run a
-- command with the sandbox @root@ bound at the project mount, the configured
-- read-only binds exposed, and the run options' env applied via @--setenv@.
-- Pure so it can be unit-tested without @bwrap@ present. @tmpDir@ is a host
-- directory to bind at @\/tmp@; @hostPath@ is the @PATH@ to expose inside.
bubblewrapArgv :: BWrapConfig -> FilePath -> FilePath -> String -> RunOptions -> (T.Text, [T.Text]) -> [String]
bubblewrapArgv bw root tmpDir hostPath opts (exe, args) =
  [ "--die-with-parent"
  , "--bind", root, mount
  , "--bind", tmpDir, "/tmp"
  , "--dev", "/dev"
  , "--proc", "/proc"
  ]
  ++ concatMap (\p -> ["--ro-bind", p, p]) (_bwrap_readOnlyBinds bw)
  ++ [ "--setenv", "PATH", hostPath
     , "--setenv", "TMPDIR", "/tmp"
     , "--chdir", mount
     ]
  ++ concatMap (\(k, v) -> ["--setenv", T.unpack k, T.unpack v]) (fromMaybe [] (_runOptions_env opts))
  ++ [T.unpack exe]
  ++ map T.unpack args
  where
    mount = _bwrap_projectMount bw

-- | A Seatbelt (@sandbox-exec@) profile (SBPL) confining file *writes* to @root@.
-- Reads are allowed broadly so system libraries\/binaries resolve; network is
-- denied unless enabled. Pure for unit-testing.
sandboxExecProfile :: SbExecConfig -> FilePath -> String
sandboxExecProfile sb root = unlines $
  [ "(version 1)"
  , "(deny default)"
  , "(allow process-exec)"
  , "(allow process-fork)"
  , "(allow sysctl-read)"
  , "(allow file-read*)"
  , "(allow file-write* (subpath \"" <> root <> "\"))"
  ]
  ++ [ "(allow network*)" | _sbExec_allowNetwork sb ]

-- | @sandbox-exec@ argv: @-p \<profile\> \<exe\> \<args…\>@ (profile inline). Pure.
sandboxExecArgv :: SbExecConfig -> String -> (T.Text, [T.Text]) -> [String]
sandboxExecArgv _ profile (exe, args) =
  ["-p", profile, T.unpack exe] ++ map T.unpack args

-- Confined execution ---------------------------------------------------------

-- | Run a 'ShellCommand' under the chosen backend, after jailing its path
-- arguments (relative-only, no @..@). The command's paths resolve under the
-- sandbox root: @bwrap@ binds @root@→mount and @--chdir@s there; the other
-- backends set the process working directory to @root@. A jail violation or a
-- spawn failure comes back as @Left@.
execSandboxed :: SandboxBackend -> FilePath -> RunOptions -> ShellCommand -> IO (Either T.Text ToolResult)
execSandboxed backend root opts cmd =
  case mapM (jail root) (shellCommandPaths cmd) of
    Left err -> pure (Left err)
    Right _ -> case backend of
      NoSandboxBackend ->
        runArgv (T.unpack exe) (map T.unpack args) (Just root) (_runOptions_env opts) (_runOptions_stdin opts)
      SandboxExecBackend sb ->
        let profile = sandboxExecProfile sb root
        in runArgv (_sbExec_exe sb) (sandboxExecArgv sb profile rendered) (Just root)
             (_runOptions_env opts) (_runOptions_stdin opts)
      BubblewrapBackend bw ->
        withSystemTempDirectory "tool-sandbox-tmp" $ \tmp -> do
          hostPath <- fromMaybe "" <$> lookupEnv "PATH"
          -- env is applied inside via --setenv, not on the bwrap process itself.
          runArgv (_bwrap_exe bw) (bubblewrapArgv bw root tmp hostPath opts rendered)
            Nothing Nothing (_runOptions_stdin opts)
  where
    rendered@(exe, args) = renderShellCommand cmd

-- | Spawn @exe@ with @argv@, capturing exit code + streams, tolerating a
-- missing executable (returned as @Left@ rather than throwing).
runArgv :: FilePath -> [String] -> Maybe FilePath -> Maybe [(T.Text, T.Text)] -> Maybe T.Text -> IO (Either T.Text ToolResult)
runArgv exe argv mCwd mEnv mStdin = do
  let cp = (proc exe argv)
        { cwd = mCwd
        , env = fmap (map (\(k, v) -> (T.unpack k, T.unpack v))) mEnv
        }
      input = maybe "" T.unpack mStdin
  result <- tryIO (readCreateProcessWithExitCode cp input)
  pure $ fmap (\(code, out, err) -> ToolResult (exitCodeToInt code) (T.pack out) (T.pack err)) result

-- Sandboxed interpreter ------------------------------------------------------

-- | Default cap for command output / file reads: 64 KiB.
defaultMaxOutputBytes :: Int
defaultMaxOutputBytes = 64 * 1024

-- | Configuration for 'runToolSandboxed'.
data SandboxConfig = SandboxConfig
  { _sandbox_root :: FilePath
    -- ^ Filesystem paths are confined under this root (path 'jail'); also the
    -- working directory the sandboxed command runs in.
  , _sandbox_maxOutputBytes :: Int
    -- ^ Truncate command output / file reads to this many characters.
  , _sandbox_backend :: SandboxBackend
    -- ^ How 'runCommand' is confined (Linux\/macOS\/none).
  , _sandbox_approve :: T.Text -> IO Bool
    -- ^ Per-call approval gate (given a human-readable description).
  , _sandbox_audit :: T.Text -> IO ()
    -- ^ Audit-log callback, invoked for every tool call.
  }

-- | A 'SandboxConfig' with the boring knobs defaulted: 'defaultMaxOutputBytes',
-- approve-everything, and a no-op audit. Override the fields you care about.
mkSandboxConfig :: FilePath -> SandboxBackend -> SandboxConfig
mkSandboxConfig root backend = SandboxConfig
  { _sandbox_root = root
  , _sandbox_maxOutputBytes = defaultMaxOutputBytes
  , _sandbox_backend = backend
  , _sandbox_approve = \_ -> pure True
  , _sandbox_audit = \_ -> pure ()
  }

-- | Sandboxed backend: 'runCommand' is confined by the configured
-- 'SandboxBackend' (with its path arguments jailed); filesystem operations run
-- host-side, jailed under the root (mirroring runGhcBWrap, which sandboxes only
-- execution and writes files host-side). Output is size-limited, every call is
-- audited, and an approval gate can veto each call.
runToolSandboxed :: forall es a. (IOE :> es) => SandboxConfig -> Eff (Tool : es) a -> Eff es a
runToolSandboxed cfg = runToolWith ToolHandlers
  { _toolHandlers_runCommand = \opts cmd -> do
      let desc = "run " <> fst (renderShellCommand cmd)
      audit ("call: " <> desc)
      ok <- approve desc
      if not ok
        then pure (ToolResult 126 "" "denied by approval gate")
        else do
          res <- liftIO (execSandboxed (_sandbox_backend cfg) (_sandbox_root cfg) opts cmd)
          case res of
            Left err -> do
              audit ("DENIED: " <> desc <> ": " <> err)
              pure (ToolResult 126 "" err)
            Right r -> pure (truncateRun r)
  , _toolHandlers_readFile = \fp -> withPath "read_file" fp (\p -> truncateRead <$> tryIO (TIO.readFile p))
  , _toolHandlers_writeFile = \fp txt -> withPath "write_file" fp (\p -> tryIO (writeFileMkdir p (TIO.writeFile p txt)))
  , _toolHandlers_appendFile = \fp txt -> withPath "append_file" fp (\p -> tryIO (writeFileMkdir p (TIO.appendFile p txt)))
  , _toolHandlers_readFileBytes = \fp -> withPath "read_file_bytes" fp (\p -> tryIO (BS.readFile p))
  , _toolHandlers_writeFileBytes = \fp bs -> withPath "write_file_bytes" fp (\p -> tryIO (writeFileMkdir p (BS.writeFile p bs)))
  , _toolHandlers_deleteFile = \fp -> withPath "delete_file" fp (\p -> tryIO (Dir.removeFile p))
  , _toolHandlers_listDirectory = \fp -> withPath "list_directory" fp (\p -> tryIO (Dir.listDirectory p))
  , _toolHandlers_makeDirectory = \fp -> withPath "make_directory" fp (\p -> tryIO (Dir.createDirectoryIfMissing True p))
  , _toolHandlers_removeDirectory = \fp -> withPath "remove_directory" fp (\p -> tryIO (Dir.removeDirectoryRecursive p))
  , _toolHandlers_copyFile = \src dst -> withPath2 "copy_file" src dst (\s d -> tryIO (writeFileMkdir d (Dir.copyFile s d)))
  , _toolHandlers_moveFile = \src dst -> withPath2 "move_file" src dst (\s d -> tryIO (writeFileMkdir d (Dir.renamePath s d)))
  , _toolHandlers_fileExists = \fp -> jailBool "file_exists" fp Dir.doesFileExist
  , _toolHandlers_directoryExists = \fp -> jailBool "directory_exists" fp Dir.doesDirectoryExist
  , _toolHandlers_pathExists = \fp -> jailBool "path_exists" fp Dir.doesPathExist
  , _toolHandlers_glob = \dir pat -> withPath "glob" dir (`globIO` pat)
  , _toolHandlers_stat = \fp -> withPath "stat" fp statIO
  }
  where
    audit msg = liftIO (_sandbox_audit cfg msg)
    approve desc = liftIO (_sandbox_approve cfg desc)
    truncateRead = either Left (Right . T.take (_sandbox_maxOutputBytes cfg))
    truncateRun r = r
      { _toolResult_stdout = T.take (_sandbox_maxOutputBytes cfg) (_toolResult_stdout r)
      , _toolResult_stderr = T.take (_sandbox_maxOutputBytes cfg) (_toolResult_stderr r)
      }
    withPath :: forall r. T.Text -> FilePath -> (FilePath -> IO (Either T.Text r)) -> Eff es (Either T.Text r)
    withPath label fp k = case jail (_sandbox_root cfg) fp of
      Left err -> audit ("DENIED (jail): " <> label <> " " <> T.pack fp) >> pure (Left err)
      Right p -> do
        audit ("call: " <> label <> " " <> T.pack p)
        ok <- approve (label <> " " <> T.pack p)
        if ok then liftIO (k p) else pure (Left "denied by approval gate")
    withPath2 :: forall r. T.Text -> FilePath -> FilePath -> (FilePath -> FilePath -> IO (Either T.Text r)) -> Eff es (Either T.Text r)
    withPath2 label src dst k = case (jail (_sandbox_root cfg) src, jail (_sandbox_root cfg) dst) of
      (Right s, Right d) -> do
        audit ("call: " <> label <> " " <> T.pack s <> " -> " <> T.pack d)
        ok <- approve (label <> " " <> T.pack s <> " -> " <> T.pack d)
        if ok then liftIO (k s d) else pure (Left "denied by approval gate")
      _ -> audit ("DENIED (jail): " <> label) >> pure (Left "path escapes sandbox root")
    jailBool :: T.Text -> FilePath -> (FilePath -> IO Bool) -> Eff es Bool
    jailBool label fp k = case jail (_sandbox_root cfg) fp of
      Left _ -> audit ("DENIED (jail): " <> label <> " " <> T.pack fp) >> pure False
      Right p -> liftIO (k p)
