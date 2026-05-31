{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The @Tool@ effect — a typed, Shelly-like surface. Commands are NOT arbitrary
-- shell strings: each is a hyper-specific, parameterized 'ShellCommand'
-- constructor with its flags as typed fields, so the command set is bounded at
-- the type level (no runtime allowlist needed). The interpreter renders a
-- 'ShellCommand' to an exec. Filesystem operations are likewise typed.
--
-- 'andThen' / 'thenAnyway' model bash @&&@ and @;@ for chaining tool runs.
--
-- Sandboxed execution (bwrap on Linux, sandbox-exec on macOS, or a portable
-- fallback) lives in "LLM.Effect.Tool.Sandbox". The command ADT below is a
-- starter set — extend it with more parameterized commands.
module LLM.Effect.Tool
  ( -- * Effect
    Tool (..)
  , ToolResult (..)
  , ShellCommand (..)
  , RunOptions (..)
  , defaultRunOptions
  , FileStat (..)
    -- * Process operations (typed, parameterized commands — no arbitrary shell)
  , runCommand
  , runCommandWith
  , renderShellCommand
  , andThen
  , thenAnyway
    -- * Filesystem operations
  , readFile
  , writeFile
  , appendFile
  , readFileBytes
  , writeFileBytes
  , deleteFile
  , listDirectory
  , makeDirectory
  , removeDirectory
  , copyFile
  , moveFile
  , fileExists
  , directoryExists
  , pathExists
  , glob
  , stat
    -- * Interpreters
  , ToolHandlers (..)
  , defaultToolHandlers
  , runToolWith
  , runToolIO
    -- * Helpers (shared with "LLM.Effect.Tool.Sandbox")
  , execShell
  , exitCodeToInt
  , tryIO
  , writeFileMkdir
  , globIO
  , statIO
  ) where

import Prelude hiding (appendFile, readFile, writeFile)

import Control.Exception (IOException, try)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.Directory as Dir
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory)
import System.Process (CreateProcess (cwd, env), proc, readCreateProcessWithExitCode)

import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret, send)

-- | The result of running a command: shell-like exit code + captured streams.
data ToolResult = ToolResult
  { _toolResult_exitCode :: Int
  , _toolResult_stdout :: T.Text
  , _toolResult_stderr :: T.Text
  } deriving (Show)

-- | The bounded, typed, parameterized set of runnable commands. Each constructor
-- is one command with its flags as fields — there is no arbitrary shell string.
-- Starter set; extend as needed.
data ShellCommand
  = Grep { _grep_pattern :: T.Text, _grep_path :: FilePath, _grep_recursive :: Bool, _grep_ignoreCase :: Bool }
  | Ls { _ls_path :: FilePath, _ls_all :: Bool, _ls_long :: Bool }
  | Find { _find_root :: FilePath, _find_namePattern :: T.Text }
  | Head { _head_path :: FilePath, _head_lines :: Int }
  | Tail { _tail_path :: FilePath, _tail_lines :: Int }
  | Wc { _wc_path :: FilePath }
  deriving (Show)

-- | Render a 'ShellCommand' to an @(executable, args)@ pair for exec.
renderShellCommand :: ShellCommand -> (T.Text, [T.Text])
renderShellCommand = \case
  Grep pat path recursive ignoreCase ->
    ("grep", concat [["-r" | recursive], ["-i" | ignoreCase], [pat, T.pack path]])
  Ls path showAll long ->
    ("ls", concat [["-a" | showAll], ["-l" | long], [T.pack path]])
  Find root namePat -> ("find", [T.pack root, "-name", namePat])
  Head path n -> ("head", ["-n", T.pack (show n), T.pack path])
  Tail path n -> ("tail", ["-n", T.pack (show n), T.pack path])
  Wc path -> ("wc", [T.pack path])

-- | Options for 'runCommandWith': stdin to feed, working directory, environment.
data RunOptions = RunOptions
  { _runOptions_stdin :: Maybe T.Text
  , _runOptions_cwd :: Maybe FilePath
  , _runOptions_env :: Maybe [(T.Text, T.Text)]
  } deriving (Show)

defaultRunOptions :: RunOptions
defaultRunOptions = RunOptions Nothing Nothing Nothing

-- | File metadata from 'stat'.
data FileStat = FileStat
  { _fileStat_size :: Integer
  , _fileStat_isDirectory :: Bool
  , _fileStat_modified :: T.Text
  } deriving (Show)

data Tool :: Effect where
  RunCommand :: ShellCommand -> Tool m ToolResult
  RunCommandWith :: RunOptions -> ShellCommand -> Tool m ToolResult
  ReadFile :: FilePath -> Tool m (Either T.Text T.Text)
  WriteFile :: FilePath -> T.Text -> Tool m (Either T.Text ())
  AppendFile :: FilePath -> T.Text -> Tool m (Either T.Text ())
  ReadFileBytes :: FilePath -> Tool m (Either T.Text ByteString)
  WriteFileBytes :: FilePath -> ByteString -> Tool m (Either T.Text ())
  DeleteFile :: FilePath -> Tool m (Either T.Text ())
  ListDirectory :: FilePath -> Tool m (Either T.Text [FilePath])
  MakeDirectory :: FilePath -> Tool m (Either T.Text ())
  RemoveDirectory :: FilePath -> Tool m (Either T.Text ())
  CopyFile :: FilePath -> FilePath -> Tool m (Either T.Text ())
  MoveFile :: FilePath -> FilePath -> Tool m (Either T.Text ())
  FileExists :: FilePath -> Tool m Bool
  DirectoryExists :: FilePath -> Tool m Bool
  PathExists :: FilePath -> Tool m Bool
  Glob :: FilePath -> T.Text -> Tool m (Either T.Text [FilePath])
  Stat :: FilePath -> Tool m (Either T.Text FileStat)

type instance DispatchOf Tool = Dynamic

-- Operations -----------------------------------------------------------------

runCommand :: (Tool :> es) => ShellCommand -> Eff es ToolResult
runCommand = send . RunCommand

runCommandWith :: (Tool :> es) => RunOptions -> ShellCommand -> Eff es ToolResult
runCommandWith opts cmd = send (RunCommandWith opts cmd)

readFile :: (Tool :> es) => FilePath -> Eff es (Either T.Text T.Text)
readFile = send . ReadFile

writeFile :: (Tool :> es) => FilePath -> T.Text -> Eff es (Either T.Text ())
writeFile fp txt = send (WriteFile fp txt)

appendFile :: (Tool :> es) => FilePath -> T.Text -> Eff es (Either T.Text ())
appendFile fp txt = send (AppendFile fp txt)

readFileBytes :: (Tool :> es) => FilePath -> Eff es (Either T.Text ByteString)
readFileBytes = send . ReadFileBytes

writeFileBytes :: (Tool :> es) => FilePath -> ByteString -> Eff es (Either T.Text ())
writeFileBytes fp bs = send (WriteFileBytes fp bs)

deleteFile :: (Tool :> es) => FilePath -> Eff es (Either T.Text ())
deleteFile = send . DeleteFile

listDirectory :: (Tool :> es) => FilePath -> Eff es (Either T.Text [FilePath])
listDirectory = send . ListDirectory

makeDirectory :: (Tool :> es) => FilePath -> Eff es (Either T.Text ())
makeDirectory = send . MakeDirectory

removeDirectory :: (Tool :> es) => FilePath -> Eff es (Either T.Text ())
removeDirectory = send . RemoveDirectory

copyFile :: (Tool :> es) => FilePath -> FilePath -> Eff es (Either T.Text ())
copyFile src dst = send (CopyFile src dst)

moveFile :: (Tool :> es) => FilePath -> FilePath -> Eff es (Either T.Text ())
moveFile src dst = send (MoveFile src dst)

fileExists :: (Tool :> es) => FilePath -> Eff es Bool
fileExists = send . FileExists

directoryExists :: (Tool :> es) => FilePath -> Eff es Bool
directoryExists = send . DirectoryExists

pathExists :: (Tool :> es) => FilePath -> Eff es Bool
pathExists = send . PathExists

glob :: (Tool :> es) => FilePath -> T.Text -> Eff es (Either T.Text [FilePath])
glob dir pat = send (Glob dir pat)

stat :: (Tool :> es) => FilePath -> Eff es (Either T.Text FileStat)
stat = send . Stat

-- | bash @&&@: run the second action only if the first exited 0; otherwise
-- short-circuit and return the first's result.
andThen :: Eff es ToolResult -> Eff es ToolResult -> Eff es ToolResult
andThen a b = a >>= \r -> if _toolResult_exitCode r == 0 then b else pure r

-- | bash @;@: run the second action regardless of the first's outcome; returns
-- the second's result (the first still runs for its effects).
thenAnyway :: Eff es ToolResult -> Eff es ToolResult -> Eff es ToolResult
thenAnyway a b = a >> b

-- Interpreters ---------------------------------------------------------------

-- | One handler per operation. Build on 'defaultToolHandlers' and override only
-- what you implement.
data ToolHandlers es = ToolHandlers
  { _toolHandlers_runCommand :: RunOptions -> ShellCommand -> Eff es ToolResult
  , _toolHandlers_readFile :: FilePath -> Eff es (Either T.Text T.Text)
  , _toolHandlers_writeFile :: FilePath -> T.Text -> Eff es (Either T.Text ())
  , _toolHandlers_appendFile :: FilePath -> T.Text -> Eff es (Either T.Text ())
  , _toolHandlers_readFileBytes :: FilePath -> Eff es (Either T.Text ByteString)
  , _toolHandlers_writeFileBytes :: FilePath -> ByteString -> Eff es (Either T.Text ())
  , _toolHandlers_deleteFile :: FilePath -> Eff es (Either T.Text ())
  , _toolHandlers_listDirectory :: FilePath -> Eff es (Either T.Text [FilePath])
  , _toolHandlers_makeDirectory :: FilePath -> Eff es (Either T.Text ())
  , _toolHandlers_removeDirectory :: FilePath -> Eff es (Either T.Text ())
  , _toolHandlers_copyFile :: FilePath -> FilePath -> Eff es (Either T.Text ())
  , _toolHandlers_moveFile :: FilePath -> FilePath -> Eff es (Either T.Text ())
  , _toolHandlers_fileExists :: FilePath -> Eff es Bool
  , _toolHandlers_directoryExists :: FilePath -> Eff es Bool
  , _toolHandlers_pathExists :: FilePath -> Eff es Bool
  , _toolHandlers_glob :: FilePath -> T.Text -> Eff es (Either T.Text [FilePath])
  , _toolHandlers_stat :: FilePath -> Eff es (Either T.Text FileStat)
  }

-- | Every operation reports "unsupported"; override the ones you implement.
defaultToolHandlers :: ToolHandlers es
defaultToolHandlers = ToolHandlers
  { _toolHandlers_runCommand = \_ _ -> pure unsupportedResult
  , _toolHandlers_readFile = \_ -> pure (Left unsupportedMsg)
  , _toolHandlers_writeFile = \_ _ -> pure (Left unsupportedMsg)
  , _toolHandlers_appendFile = \_ _ -> pure (Left unsupportedMsg)
  , _toolHandlers_readFileBytes = \_ -> pure (Left unsupportedMsg)
  , _toolHandlers_writeFileBytes = \_ _ -> pure (Left unsupportedMsg)
  , _toolHandlers_deleteFile = \_ -> pure (Left unsupportedMsg)
  , _toolHandlers_listDirectory = \_ -> pure (Left unsupportedMsg)
  , _toolHandlers_makeDirectory = \_ -> pure (Left unsupportedMsg)
  , _toolHandlers_removeDirectory = \_ -> pure (Left unsupportedMsg)
  , _toolHandlers_copyFile = \_ _ -> pure (Left unsupportedMsg)
  , _toolHandlers_moveFile = \_ _ -> pure (Left unsupportedMsg)
  , _toolHandlers_fileExists = \_ -> pure False
  , _toolHandlers_directoryExists = \_ -> pure False
  , _toolHandlers_pathExists = \_ -> pure False
  , _toolHandlers_glob = \_ _ -> pure (Left unsupportedMsg)
  , _toolHandlers_stat = \_ -> pure (Left unsupportedMsg)
  }
  where
    unsupportedResult = ToolResult 127 "" "tool not supported by this interpreter"
    unsupportedMsg = "tool not supported by this interpreter"

runToolWith :: ToolHandlers es -> Eff (Tool : es) a -> Eff es a
runToolWith h = interpret $ \_ -> \case
  RunCommand cmd -> _toolHandlers_runCommand h defaultRunOptions cmd
  RunCommandWith opts cmd -> _toolHandlers_runCommand h opts cmd
  ReadFile fp -> _toolHandlers_readFile h fp
  WriteFile fp txt -> _toolHandlers_writeFile h fp txt
  AppendFile fp txt -> _toolHandlers_appendFile h fp txt
  ReadFileBytes fp -> _toolHandlers_readFileBytes h fp
  WriteFileBytes fp bs -> _toolHandlers_writeFileBytes h fp bs
  DeleteFile fp -> _toolHandlers_deleteFile h fp
  ListDirectory fp -> _toolHandlers_listDirectory h fp
  MakeDirectory fp -> _toolHandlers_makeDirectory h fp
  RemoveDirectory fp -> _toolHandlers_removeDirectory h fp
  CopyFile src dst -> _toolHandlers_copyFile h src dst
  MoveFile src dst -> _toolHandlers_moveFile h src dst
  FileExists fp -> _toolHandlers_fileExists h fp
  DirectoryExists fp -> _toolHandlers_directoryExists h fp
  PathExists fp -> _toolHandlers_pathExists h fp
  Glob dir pat -> _toolHandlers_glob h dir pat
  Stat fp -> _toolHandlers_stat h fp

-- | Real backend: subprocesses + the local filesystem. No command allowlist
-- needed — the 'ShellCommand' ADT already bounds what can run. Writes create
-- parent directories.
runToolIO :: (IOE :> es) => Eff (Tool : es) a -> Eff es a
runToolIO = runToolWith ToolHandlers
  { _toolHandlers_runCommand = \opts cmd -> liftIO (execShell opts cmd)
  , _toolHandlers_readFile = \fp -> liftIO (tryIO (TIO.readFile fp))
  , _toolHandlers_writeFile = \fp txt -> liftIO (tryIO (writeFileMkdir fp (TIO.writeFile fp txt)))
  , _toolHandlers_appendFile = \fp txt -> liftIO (tryIO (writeFileMkdir fp (TIO.appendFile fp txt)))
  , _toolHandlers_readFileBytes = \fp -> liftIO (tryIO (BS.readFile fp))
  , _toolHandlers_writeFileBytes = \fp bs -> liftIO (tryIO (writeFileMkdir fp (BS.writeFile fp bs)))
  , _toolHandlers_deleteFile = \fp -> liftIO (tryIO (Dir.removeFile fp))
  , _toolHandlers_listDirectory = \fp -> liftIO (tryIO (Dir.listDirectory fp))
  , _toolHandlers_makeDirectory = \fp -> liftIO (tryIO (Dir.createDirectoryIfMissing True fp))
  , _toolHandlers_removeDirectory = \fp -> liftIO (tryIO (Dir.removeDirectoryRecursive fp))
  , _toolHandlers_copyFile = \src dst -> liftIO (tryIO (writeFileMkdir dst (Dir.copyFile src dst)))
  , _toolHandlers_moveFile = \src dst -> liftIO (tryIO (writeFileMkdir dst (Dir.renamePath src dst)))
  , _toolHandlers_fileExists = \fp -> liftIO (Dir.doesFileExist fp)
  , _toolHandlers_directoryExists = \fp -> liftIO (Dir.doesDirectoryExist fp)
  , _toolHandlers_pathExists = \fp -> liftIO (Dir.doesPathExist fp)
  , _toolHandlers_glob = \dir pat -> liftIO (globIO dir pat)
  , _toolHandlers_stat = \fp -> liftIO (statIO fp)
  }

-- Internals ------------------------------------------------------------------

execShell :: RunOptions -> ShellCommand -> IO ToolResult
execShell opts cmd = do
  let (exe, args) = renderShellCommand cmd
      cp =
        (proc (T.unpack exe) (map T.unpack args))
          { cwd = _runOptions_cwd opts
          , env = fmap (map (\(k, v) -> (T.unpack k, T.unpack v))) (_runOptions_env opts)
          }
      input = maybe "" T.unpack (_runOptions_stdin opts)
  (code, out, err) <- readCreateProcessWithExitCode cp input
  pure (ToolResult (exitCodeToInt code) (T.pack out) (T.pack err))

-- | Map an 'ExitCode' to the conventional integer (0 = success).
exitCodeToInt :: ExitCode -> Int
exitCodeToInt ExitSuccess = 0
exitCodeToInt (ExitFailure n) = n

writeFileMkdir :: FilePath -> IO a -> IO a
writeFileMkdir fp act = Dir.createDirectoryIfMissing True (takeDirectory fp) >> act

globIO :: FilePath -> T.Text -> IO (Either T.Text [FilePath])
globIO dir pat = do
  result <- tryIO (Dir.listDirectory dir)
  pure $ case result of
    Left err -> Left err
    Right entries -> Right (filter (globMatch (T.unpack pat)) entries)

statIO :: FilePath -> IO (Either T.Text FileStat)
statIO fp = tryIO $ do
  isDir <- Dir.doesDirectoryExist fp
  size <- if isDir then pure 0 else Dir.getFileSize fp
  modified <- Dir.getModificationTime fp
  pure (FileStat size isDir (T.pack (show modified)))

tryIO :: IO a -> IO (Either T.Text a)
tryIO action = do
  result <- try action
  pure $ case result of
    Left (e :: IOException) -> Left (T.pack (show e))
    Right a -> Right a

-- | Minimal glob matcher: @*@ matches any sequence, @?@ matches any one char.
globMatch :: String -> String -> Bool
globMatch [] [] = True
globMatch ('*' : ps) cs = globMatch ps cs || (not (null cs) && globMatch ('*' : ps) (drop 1 cs))
globMatch ('?' : ps) (_ : cs) = globMatch ps cs
globMatch (p : ps) (c : cs) = p == c && globMatch ps cs
globMatch _ _ = False
