{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Pure tests for the effect layer. Everything here runs via 'runPureEff' —
-- no network, no IO — by interpreting the LLM effect with the mock backend and
-- Memory/Tool over their pure interpreters. Dependency-light on purpose (just
-- base/text/effectful-core/the lib): a hand-rolled harness, no test framework.
module Main (main) where

import Control.Monad (forM_, unless)
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (exitFailure)

import Effectful (runPureEff)
import Effectful.State.Static.Local (evalState)

import qualified Control.Exception as CE
import LLM.Capability (askText)
import LLM.Effect (askMultimodal, askWithContext)
import LLM.Provider.AnthropicCli
  ( AskArgs (..)
  , askArgs
  , defaultAskArgs
  , multimodalAskArgs
  , readPreamble
  )
-- Importing these stub modules brings the deferred capability instances
-- into scope so the throw-on-touch tests can typecheck. The modules
-- themselves export nothing other than their instances.
import LLM.Provider.AnthropicHttpStub ()
import LLM.Provider.OpenAIHttpStub ()
import LLM.Effect.Memory
  ( emptyMemoryStore
  , pin
  , prune
  , recallAll
  , remember
  , runMemoryState
  )
import LLM.Effect.Mock (runLLMMock)
import LLM.Effect.Tool
  ( ShellCommand (..)
  , ToolHandlers (..)
  , ToolResult (..)
  , defaultRunOptions
  , defaultToolHandlers
  , renderShellCommand
  , runCommand
  , runToolWith
  )
import LLM.Effect.Tool.Sandbox
  ( bubblewrapArgv
  , defaultBWrapConfig
  , defaultSbExecConfig
  , jail
  , sandboxExecProfile
  , shellCommandPaths
  )
import Data.List (isInfixOf)
import LLM.Types
  ( APIProvider (..)
  , ContentWithRole (..)
  , ConvoAnswer (..)
  , ConvoQuery (..)
  , ConvoQuestion (..)
  , GPTRole (User)
  , RelevantContext (..)
  , Tag (..)
  , cwr
  )

main :: IO ()
main = do
  -- The stub instances' method bodies reduce immediately to a pure
  -- 'error' (via 'claudeDeferredLogicImplementation'). Forcing the
  -- returned 'Eff' value to WHNF therefore raises the exception even
  -- without an interpreter — what matters is that the binding is
  -- bottom, not that the Eff action ever runs. We run it through
  -- 'runPureEff' against an empty effect row only to fix the
  -- otherwise-ambiguous @es@; because the body never inspects the
  -- effect row, this still triggers the underlying 'error'.
  let anthropicProbe :: Text
      anthropicProbe = runPureEff
        . evalState emptyMemoryStore
        . runMemoryState
        . runLLMMock @'AnthropicHttp (\_ -> Right "n/a")
        $ askText @'AnthropicHttp "x"
      openAIProbe    :: Text
      openAIProbe    = runPureEff
        . evalState emptyMemoryStore
        . runMemoryState
        . runLLMMock @'OpenAIHttp (\_ -> Right "n/a")
        $ askText @'OpenAIHttp "x"
  anthropicHttpStubThrows <- throws (CE.evaluate anthropicProbe)
  openAIHttpStubThrows    <- throws (CE.evaluate openAIProbe)
  let checks =
        [ ("memory: remember/recallAll round-trip", memCount == 2)
        , ("memory: pinned turn survives prune", pinnedTags == ["keep"])
        , ("effect: context loop records each turn", ctxTurns == 2)
        , ("tool: runCommand with pure handlers", toolStdout == "ls")
        , ("sandbox: jail accepts relative, rejects absolute + ..", jailOk)
        , ("sandbox: shellCommandPaths extracts the path", shellCommandPaths (Grep "x" "sub/dir" False False) == ["sub/dir"])
        , ("sandbox: bwrap argv binds root + ro-binds store + runs exe", bwrapArgvOk)
        , ("sandbox: sandbox-exec profile confines writes to root", sbProfileOk)
        , ("anthropic-cli: readPreamble [] is empty", readPreamble [] == "")
        , ("anthropic-cli: readPreamble [p] uses Read tool wording"
          , readPreamble ["/img/a.png"] == "Read the image file at /img/a.png using your Read tool.\n\n")
        , ("anthropic-cli: readPreamble [p1,p2] enumerates as bullets", multiPreambleOk)
        , ("anthropic-cli: askArgs emits --add-dir=DIR (equals-bound)", addDirEqualsBound)
        , ("anthropic-cli: askArgs prepends -p and skip-perms", argvFrontMatter)
        , ("anthropic-http: askText stub throws on touch", anthropicHttpStubThrows)
        , ("openai-http: askText stub throws on touch", openAIHttpStubThrows)
        , ("anthropic-cli: multimodalAskArgs binds --add-dir per unique parent dir", multimodalArgsDirsOk)
        , ("anthropic-cli: multimodalAskArgs prepends Read-tool preamble to prompt", multimodalArgsPreambleOk)
        , ("effect: AskMultimodal routes through Mock backend's responder", askMultimodalRoutesOk)
        ]
  forM_ checks $ \(name, ok) ->
    putStrLn $ (if ok then "PASS  " else "FAIL  ") <> name
  unless (all snd checks) exitFailure

-- Two remembered turns should both be recallable.
memCount :: Int
memCount = runPureEff . evalState emptyMemoryStore . runMemoryState $ do
  remember (ConvoQuery (Tag "a") (ConvoQuestion []) (ConvoAnswer "first"))
  remember (ConvoQuery (Tag "b") (ConvoQuestion []) (ConvoAnswer "second"))
  length <$> recallAll

-- prune 0 drops everything unpinned; the pinned turn must remain.
pinnedTags :: [Text]
pinnedTags = runPureEff . evalState emptyMemoryStore . runMemoryState $ do
  remember (ConvoQuery (Tag "keep") (ConvoQuestion []) (ConvoAnswer "x"))
  pin (Tag "keep")
  remember (ConvoQuery (Tag "t1") (ConvoQuestion []) (ConvoAnswer "y"))
  remember (ConvoQuery (Tag "t2") (ConvoQuestion []) (ConvoAnswer "y"))
  prune 0
  map (unTag . _convoQuery_tag) <$> recallAll

-- Two context-aware asks against the mock provider should leave two turns in
-- memory. Fully pure: mock LLM + Memory + State, run with runPureEff.
ctxTurns :: Int
ctxTurns =
  runPureEff
    . evalState emptyMemoryStore
    . runMemoryState
    . runLLMMock @'AnthropicHttp (\_ -> Right "ok")
    $ do
        _ <- askWithContext @'AnthropicHttp (LastN 10) (Tag "q1", ConvoQuestion [cwr User "hi"])
        _ <- askWithContext @'AnthropicHttp (LastN 10) (Tag "q2", ConvoQuestion [cwr User "again"])
        length <$> recallAll

-- A custom (pure) Tool backend: stdout is just the rendered command name.
-- Uses defaultToolHandlers so only the one handler we exercise is overridden.
toolStdout :: Text
toolStdout = runPureEff . runToolWith handlers $ do
  result <- runCommand (Ls "." True False)
  pure (_toolResult_stdout result)
  where
    handlers =
      defaultToolHandlers
        { _toolHandlers_runCommand = \_opts cmd -> pure (ToolResult 0 (fst (renderShellCommand cmd)) "")
        }

-- Sandbox path jail: relative paths pass (rebased under root); absolute paths
-- and '..' traversal are rejected.
jailOk :: Bool
jailOk =
  jail "/root" "sub/file.txt" == Right "/root/sub/file.txt"
    && isLeft (jail "/root" "/etc/passwd")
    && isLeft (jail "/root" "../escape")
  where
    isLeft = either (const True) (const False)

-- The bwrap argv must bind the sandbox root at the project mount, ro-bind the
-- configured store path, and end by invoking the rendered command.
bwrapArgvOk :: Bool
bwrapArgvOk =
  all (`elem` argv) ["--bind", "/srv/root", "/project", "--ro-bind", "/nix/store", "--chdir", "ls"]
  where
    argv = bubblewrapArgv defaultBWrapConfig "/srv/root" "/tmp/box" "/usr/bin:/bin"
             defaultRunOptions (renderShellCommand (Ls "." True False))

-- The generated Seatbelt profile must confine writes to the root and (with the
-- default config) deny network.
sbProfileOk :: Bool
sbProfileOk =
  ("(allow file-write* (subpath \"/srv/root\"))" `isInfixOf` profile)
    && not ("(allow network*)" `isInfixOf` profile)
  where
    profile = sandboxExecProfile defaultSbExecConfig "/srv/root"

-- A two-path preamble must enumerate the paths as a bullet list inside the
-- "Read the following image files" header.
multiPreambleOk :: Bool
multiPreambleOk =
  let p = readPreamble ["/a/x.png", "/a/y.png"]
  in "Read the following image files" `T.isInfixOf` p
       && "  - /a/x.png" `T.isInfixOf` p
       && "  - /a/y.png" `T.isInfixOf` p
  where
    -- shadowed import to keep this self-contained without changing the
    -- top of the file
    _shadow = ()

-- Two-path askArgs must emit BOTH --add-dir flags in the equals-bound form
-- ("--add-dir=DIR"), never as separate "--add-dir" "DIR" tokens, because the
-- CLI's argparse otherwise greedily consumes the following positional prompt.
addDirEqualsBound :: Bool
addDirEqualsBound =
  let a   = (defaultAskArgs "hello") { aAddDirs = ["/srv/a", "/srv/b"] }
      av  = askArgs a
  in    "--add-dir=/srv/a" `elem` av
     && "--add-dir=/srv/b" `elem` av
     && "--add-dir" `notElem` av  -- bare form must not appear
     && last av == "hello"        -- prompt is last positional

-- The default args must put -p and --dangerously-skip-permissions in front of
-- the prompt body.
argvFrontMatter :: Bool
argvFrontMatter =
  let av = askArgs (defaultAskArgs "hello")
  in take 2 av == ["-p", "--dangerously-skip-permissions"]
       && last av == "hello"

-- Two-image multimodal: --add-dir must appear once per unique parent dir
-- (equals-bound), in the same order as nub-deduped takeDirectory.
multimodalArgsDirsOk :: Bool
multimodalArgsDirsOk =
  let a   = multimodalAskArgs ["/srv/a/1.png", "/srv/a/2.png", "/srv/b/3.png"] [cwr User "go"]
      av  = askArgs a
  in    "--add-dir=/srv/a" `elem` av
     && "--add-dir=/srv/b" `elem` av
     && length (filter (== "--add-dir=/srv/a") av) == 1
     && aAddDirs a == ["/srv/a", "/srv/b"]

-- The constructed prompt body must lead with the Read-tool preamble and
-- carry the flattened user contents after it.
multimodalArgsPreambleOk :: Bool
multimodalArgsPreambleOk =
  let a = multimodalAskArgs ["/img/x.png"] [cwr User "describe please"]
  in    "Read the image file at /img/x.png using your Read tool." `T.isInfixOf` aPrompt a
     && "describe please" `T.isInfixOf` aPrompt a

-- Routing 'askMultimodal' through the Mock interpreter must reach the
-- responder with the text contents intact (the image carrier is dropped
-- by the mock, by design — the assertion is on the text path).
askMultimodalRoutesOk :: Bool
askMultimodalRoutesOk =
  let result = runPureEff
        . evalState emptyMemoryStore
        . runMemoryState
        . runLLMMock @'AnthropicCli (\contents ->
            if any (\c -> "frame.png" `T.isInfixOf` _cwr_content c) contents
              then Right "saw-frame"
              else Right "no-frame")
        $ askMultimodal @'AnthropicCli ["/img/frame.png"] [cwr User "frame.png is the image"]
  in result == Right "saw-frame"

-- 'CE.evaluate'-driven check: was an exception raised when forcing the action?
-- The stub modules' 'claudeDeferredLogicImplementation' throws a pure 'error',
-- so the underlying SomeException catches it.
throws :: forall a. IO a -> IO Bool
throws m = do
  r <- CE.try m :: IO (Either CE.SomeException a)
  pure (either (const True) (const False) r)
