{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Living, COMPILE-ONLY examples of wiring the effects together.
--
-- These use the REAL provider interpreter ('runLLMAnthropic') with a PLACEHOLDER
-- API key, so the file type-checks the real request-construction path (catching
-- any API drift) without needing a key or network. It is built by nix but never
-- run — with the fake key the provider calls would return 401 (and there is no
-- network in CI). Set a real key and run it yourself to see actual replies.
--
-- It demonstrates: the full tool loop ('askTooledAnthropic') over a sandboxed
-- 'Tool', history compaction, the one-round 'stepTooled' primitive, the
-- graph-structured 'MemoryGraph', and fetching + reading provider metadata.
module Main (main) where

import Effectful (runEff, runPureEff)
import Effectful.Reader.Static (asks)
import Effectful.State.Static.Local (evalState)
import Network.HTTP.Client.TLS (newTlsManager)

import LLM.Effect.Anthropic (runLLMAnthropic)
import LLM.Effect.Compaction (compactAnthropic)
import LLM.Effect.Memory (emptyMemoryStore, runMemoryState)
import LLM.Effect.MemoryGraph
  ( addTurn
  , emptyMemoryGraph
  , recallRecent
  , relateTurns
  , runMemoryGraphState
  )
import LLM.Effect.Tool.Sandbox (detectBackend, mkSandboxConfig, runToolSandboxed)
import LLM.Provider.Anthropic (defaultClaudeConfig, fetchClaudeModels)
import LLM.ProviderMeta (modelIds, runProviderMeta)
import LLM.Tooling
  ( appendToolRound
  , askTooledAnthropic
  , defaultToolSet
  , runToolSet
  , stepTooled
  , toolDefs
  )
import LLM.Types
  ( APIKey (..)
  , APIProvider (..)
  , Block (..)
  , GPTRole (..)
  , RichMessage (..)
  , cwr
  )

main :: IO ()
main = do
  mgr <- newTlsManager
  let key = APIKey "sk-ant-placeholder" :: APIKey 'AnthropicHttp
      cfg = defaultClaudeConfig key mgr -- override fields with record-update if desired
  backend <- detectBackend
  let sandboxCfg = mkSandboxConfig "/tmp/llm-with-context-sandbox" backend

  -- 1. Full tool loop against a sandboxed Tool, then compact the history.
  --    Stack (innermost first): LLM 'AnthropicHttp, Tool, Reader ToolSet, Memory,
  --    State MemoryStore, IOE.
  answer <- runEff
    . evalState emptyMemoryStore
    . runMemoryState
    . runToolSet defaultToolSet
    . runToolSandboxed sandboxCfg
    . runLLMAnthropic cfg
    $ do
        a <- askTooledAnthropic [cwr User "List the project files, then summarise them."]
        compactAnthropic
        pure a
  putStrLn ("tool-loop answer: " <> show answer)

  -- 2. The one-round primitive: the caller drives the loop. One step asks once,
  --    dispatches the tool calls, and hands back the raw turn + result blocks.
  let firstMsgs = [RichMessage User [BlockText "Find the TODO comments."]]
  step <- runEff
    . evalState emptyMemoryStore
    . runMemoryState
    . runToolSandboxed sandboxCfg
    . runLLMAnthropic cfg
    $ stepTooled @'AnthropicHttp toolDefs firstMsgs
  case step of
    Left err -> putStrLn ("step error: " <> show err)
    Right (turn, resultBlocks) ->
      -- Assemble the next round; loop again with this if you want to continue.
      let _nextMsgs = appendToolRound firstMsgs turn resultBlocks
      in putStrLn "stepTooled: completed one tool round"

  -- 3. Graph-structured conversation memory (pure — no network).
  let recent = runPureEff
        . evalState emptyMemoryGraph
        . runMemoryGraphState
        $ do
            u <- addTurn "user: hello"
            r <- addTurn "assistant: hi there"
            relateTurns u "answeredBy" r
            recallRecent 10
  putStrLn ("memory-graph recent turns: " <> show recent)

  -- 4. Provider metadata: fetch the model list once, then read it from the
  --    read-only env. (With the fake key the fetch returns an error.)
  meta <- fetchClaudeModels cfg
  case meta of
    Left err -> putStrLn ("model fetch error (expected with placeholder key): " <> show err)
    Right pm -> do
      ids <- runEff . runProviderMeta pm $ asks modelIds
      putStrLn ("available models: " <> show ids)
