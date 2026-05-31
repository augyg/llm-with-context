{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Pure tests for the effect layer. Everything here runs via 'runPureEff' —
-- no network, no IO — by interpreting the LLM effect with the mock backend and
-- Memory/Tool over their pure interpreters. Dependency-light on purpose (just
-- base/text/effectful-core/the lib): a hand-rolled harness, no test framework.
module Main (main) where

import Control.Monad (forM_, unless)
import Data.Text (Text)
import System.Exit (exitFailure)

import Effectful (runPureEff)
import Effectful.State.Static.Local (evalState)

import LLM.Effect (askWithContext)
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
  , GPTAnswer (..)
  , GPTQuery (..)
  , GPTQuestion (..)
  , GPTRole (User)
  , RelevantContext (..)
  , Tag (..)
  , cwr
  )

main :: IO ()
main = do
  let checks =
        [ ("memory: remember/recallAll round-trip", memCount == 2)
        , ("memory: pinned turn survives prune", pinnedTags == ["keep"])
        , ("effect: context loop records each turn", ctxTurns == 2)
        , ("tool: runCommand with pure handlers", toolStdout == "ls")
        , ("sandbox: jail accepts relative, rejects absolute + ..", jailOk)
        , ("sandbox: shellCommandPaths extracts the path", shellCommandPaths (Grep "x" "sub/dir" False False) == ["sub/dir"])
        , ("sandbox: bwrap argv binds root + ro-binds store + runs exe", bwrapArgvOk)
        , ("sandbox: sandbox-exec profile confines writes to root", sbProfileOk)
        ]
  forM_ checks $ \(name, ok) ->
    putStrLn $ (if ok then "PASS  " else "FAIL  ") <> name
  unless (all snd checks) exitFailure

-- Two remembered turns should both be recallable.
memCount :: Int
memCount = runPureEff . evalState emptyMemoryStore . runMemoryState $ do
  remember (GPTQuery (Tag "a") (GPTQuestion []) (GPTAnswer "first"))
  remember (GPTQuery (Tag "b") (GPTQuestion []) (GPTAnswer "second"))
  length <$> recallAll

-- prune 0 drops everything unpinned; the pinned turn must remain.
pinnedTags :: [Text]
pinnedTags = runPureEff . evalState emptyMemoryStore . runMemoryState $ do
  remember (GPTQuery (Tag "keep") (GPTQuestion []) (GPTAnswer "x"))
  pin (Tag "keep")
  remember (GPTQuery (Tag "t1") (GPTQuestion []) (GPTAnswer "y"))
  remember (GPTQuery (Tag "t2") (GPTQuestion []) (GPTAnswer "y"))
  prune 0
  map (unTag . _gptQuery_tag) <$> recallAll

-- Two context-aware asks against the mock provider should leave two turns in
-- memory. Fully pure: mock LLM + Memory + State, run with runPureEff.
ctxTurns :: Int
ctxTurns =
  runPureEff
    . evalState emptyMemoryStore
    . runMemoryState
    . runLLMMock @'Anthropic (\_ -> Right "ok")
    $ do
        _ <- askWithContext @'Anthropic (LastN 10) (Tag "q1", GPTQuestion [cwr User "hi"])
        _ <- askWithContext @'Anthropic (LastN 10) (Tag "q2", GPTQuestion [cwr User "again"])
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
