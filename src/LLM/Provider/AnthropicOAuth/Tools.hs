{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Description: OAuth-authenticated tool-use loop for Anthropic's
-- @POST \/v1\/messages@. Phase B.5.4.
-- Copyright: (c) lazyLambda, 2026
-- License: MIT
-- Maintainer: galen.sprout@gmail.com
--
-- Mirrors "LLM.Provider.AnthropicOAuth.Ask"'s HTTP flow but
-- translates to the tool-request wire shape (@tools@ array +
-- @messages@ with @tool_use@/@tool_result@ content blocks) via
-- the already-exported 'toolDefJSON' \/ 'richMessageJSON' \/
-- 'responseToToolTurn' helpers from "LLM.Provider.Anthropic".
--
-- Two public entry points:
--
--   * 'askAnthropicOAuthTools' — single POST with tool definitions.
--     Returns a 'ToolTurn' describing the assistant's response
--     (text and\/or tool_use blocks). Does NOT loop.
--   * 'runAnthropicOAuthToolLoop' — the full agentic loop:
--     ask → execute tools → append tool_result → repeat until no
--     tool_use OR a round cap is reached.
--
-- The permission gate lives in the executor callback (the harness
-- wraps 'defaultToolExecutor' with 'mkGatedExecutor' before passing
-- it in), so this module stays policy-agnostic.
module LLM.Provider.AnthropicOAuth.Tools
  ( askAnthropicOAuthTools
  , runAnthropicOAuthToolLoop
  ) where

import Control.Concurrent.MVar (readMVar)
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
  ( Request (..)
  , RequestBody (RequestBodyLBS)
  , Response (..)
  , httpLbs
  , parseRequest
  )
import Network.HTTP.Types.Header (hAuthorization, hContentType, hUserAgent)
import Network.HTTP.Types.Status (statusCode, statusMessage)

import LLM.Provider.Anthropic
  ( AnthropicAPIError (..)
  , AnthropicErrorResponse (..)
  , AnthropicResponse
  , responseToToolTurn
  , richMessageJSON
  , toolDefJSON
  )
import LLM.Provider.AnthropicOAuth.Ask
  ( AnthropicOAuthEnv (..)
  , defaultAnthropicBeta
  , defaultAnthropicVersion
  , defaultUserAgent
  , ensureFreshCredentials
  )
import LLM.Provider.AnthropicOAuth.Credentials (Credentials (..))
import LLM.Types
  ( Block (..)
  , GPTRole (..)
  , RichMessage (..)
  , ToolDef
  , ToolResult (..)
  , ToolTurn (..)
  , ToolUse (..)
  )

-- | Single-shot tool-aware request: send @tools@ + the current
-- 'RichMessage' history, decode the response into a 'ToolTurn'
-- (text or tool_use blocks). Doesn't loop.
--
-- 'Left' on transport failure, non-2xx, decode error, or persistent
-- 401 after a single refresh (same semantics as
-- 'askAnthropicOAuth').
askAnthropicOAuthTools
  :: MonadIO m
  => AnthropicOAuthEnv
  -> [ToolDef]
  -> [RichMessage]
  -> m (Either Text ToolTurn)
askAnthropicOAuthTools env defs msgs = liftIO $ do
  freshness <- ensureFreshCredentials env
  case freshness of
    Left err -> pure (Left err)
    Right () -> do
      creds <- readMVar (envCredentials env)
      postToolsOnce env creds defs msgs

postToolsOnce
  :: AnthropicOAuthEnv
  -> Credentials
  -> [ToolDef]
  -> [RichMessage]
  -> IO (Either Text ToolTurn)
postToolsOnce env creds defs msgs = do
  mkReq <- try (parseRequest (T.unpack (envAnthropicBaseUrl env <> "/v1/messages")))
    :: IO (Either SomeException Request)
  case mkReq of
    Left e -> pure (Left ("Anthropic OAuth tools: HTTP setup: " <> T.pack (show e)))
    Right baseReq -> do
      let toolsJSON = [toolDefJSON False d | d <- defs]
            -- 'False' here = cache_control OFF per tool. The OAuth
            -- backend doesn't currently thread the prompt-cache flag
            -- (ClaudeConfig's '_claudeConfig_promptCaching' lives in the
            -- API-key path only). Prompt caching for OAuth would need a
            -- flag on 'AnthropicOAuthEnv' — tracked as a future task.
          body = Aeson.object
            [ "model"      .= envModel env
            , "max_tokens" .= envMaxTokens env
            , "tools"      .= toolsJSON
            , "messages"   .= map richMessageJSON msgs
            ]
          req = baseReq
            { method         = "POST"
            , requestHeaders =
                [ (hAuthorization,     "Bearer " <> TE.encodeUtf8 (credAccessToken creds))
                , ("anthropic-version", TE.encodeUtf8 defaultAnthropicVersion)
                , ("anthropic-beta",    TE.encodeUtf8 defaultAnthropicBeta)
                , (hUserAgent,          TE.encodeUtf8 defaultUserAgent)
                , (hContentType,        "application/json")
                ]
            , requestBody    = RequestBodyLBS (Aeson.encode body)
            }
      httpResult :: Either SomeException (Response BL.ByteString) <- try (httpLbs req (envManager env))
      case httpResult of
        Left e -> pure (Left ("Anthropic OAuth tools: HTTP exception: " <> T.pack (show e)))
        Right r -> pure (classify r)
  where
    classify :: Response BL.ByteString -> Either Text ToolTurn
    classify r
      | code >= 200, code < 300 =
          case Aeson.eitherDecode bs :: Either String AnthropicResponse of
            Left e   -> Left ("Anthropic OAuth tools: decode: " <> T.pack e)
            Right rs -> Right (responseToToolTurn rs)
      | code == 401 =
          -- Caller is expected to proactively refresh via
          -- 'ensureFreshCredentials' before entering this function;
          -- a 401 here means the token was invalidated between the
          -- freshness check and the request. Surface so the caller
          -- can decide to retry at a higher level.
          Left "Anthropic OAuth tools: 401 Unauthorized (run `claude login` + retry if persistent)"
      | otherwise =
          case Aeson.eitherDecode bs :: Either String AnthropicErrorResponse of
            Right errResp -> Left
              ( "Anthropic API error ("
                  <> T.pack (show code) <> " "
                  <> _aerr_type (_aerrResp_error errResp) <> "): "
                  <> _aerr_message (_aerrResp_error errResp)
              )
            Left _ -> Left
              ( "Anthropic non-success status "
                  <> T.pack (show code) <> " "
                  <> TE.decodeUtf8 (statusMessage (responseStatus r))
              )
      where
        code = statusCode (responseStatus r)
        bs   = responseBody r

-- | Full agentic tool-use loop for OAuth. Parallels
-- 'LLM.Provider.runToolLoop' but speaks the Anthropic Messages API
-- (tool_use/tool_result content blocks, not OpenAI's function
-- calls) and authenticates via Bearer tokens.
--
-- Terminates on:
--
--   * assistant turn with no tool_use blocks → final text returned
--     (Right)
--   * round cap reached → whatever text accumulated so far (Right)
--   * transport / decode / API error at any iteration → (Left)
--
-- The @execTool@ callback IS the permission-gated executor the
-- harness builds (see 'Main.mkGatedExecutor') — denies render as
-- synthetic ToolResult rows, which the model sees as "tool said:
-- Denied by user" and typically abandons that path.
runAnthropicOAuthToolLoop
  :: MonadIO m
  => AnthropicOAuthEnv
  -> Int                                   -- ^ max rounds
  -> [ToolDef]
  -> ([ToolUse] -> IO [ToolResult])         -- ^ gated executor
  -> [RichMessage]                          -- ^ initial messages (incl. user turn)
  -> m (Either Text Text)
runAnthropicOAuthToolLoop env maxRounds defs execTool initial =
  liftIO (go 0 initial [])
  where
    go :: Int -> [RichMessage] -> [Text] -> IO (Either Text Text)
    go n history acc
      | n >= maxRounds =
          pure $ Right $ T.unlines $
            "[tool-loop] reached max rounds; partial progress:" : reverse acc
      | otherwise = do
          res <- askAnthropicOAuthTools env defs history
          case res of
            Left err -> pure (Left err)
            Right turn -> do
              let uses = _toolTurn_toolUses turn
                  mTurnText = _toolTurn_text turn
              if null uses
                then pure $ Right $ case mTurnText of
                  Just t  -> t
                  Nothing -> T.unlines (reverse acc)
                else do
                  results <- execTool uses
                  let assistantMsg =
                        RichMessage Assistant (_toolTurn_assistantBlocks turn)
                      toolResultMsg =
                        RichMessage User
                          [ BlockToolResult (_toolResult_callId r)
                                             (_toolResult_content r)
                                             False
                          | r <- results
                          ]
                      acc'  = case mTurnText of
                        Just t  -> t : acc
                        Nothing -> acc
                      acc'' = map _toolResult_content results ++ acc'
                  go (n + 1) (history ++ [assistantMsg, toolResultMsg]) acc''
