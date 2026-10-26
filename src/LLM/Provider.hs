{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Description: Provider-agnostic LLM execution layer.
-- Copyright: (c) lazyLambda, 2024-2026
-- License: MIT
-- Maintainer: galen.sprout@gmail.com
--
-- Defines the 'LLMT' (stateless) and 'ConvoT' (stateful conversation) monad
-- transformers, backend dispatch via 'askBackend', and convenience functions
-- for JSON and Parsec extraction from LLM responses.
module LLM.Provider
  ( -- * Provider identifiers
    WebProvider(..)
    -- * Backend configuration
  , LLMAPI(..)
  , LLMBackend(..)
  , LLMEnv(..)
    -- * Errors
  , LLMError(..)
    -- * Monad transformers
  , LLMT(..)
  , ConvoT(..)
    -- * Running
  , runLLM
  , runConvo
    -- * Querying
  , askBackend
  , askLLM
  , askLLMJSON
  , askLLMParsec
  , parseLLMJSON
    -- * Tool support
  , ToolResponse(..)
  , askOpenAITools
  , runToolLoop
    -- * Env helpers (from gs/gets-context — runLLM / runConvo already listed above)
  , checkArgMax
  , mkLLMEnv
  ) where

import LLM.Types
import LLM.ReadLLM (ReadLLM(..))
import Scrappy.JSON.Value (FromJValue, fromJValue, parseJValue)
import Scrappy.Scrape (scrapeFirst')
import Scrappy.Types (Html)
import Data.String (fromString)

import qualified Control.Exception as CE
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Trans.State (StateT, evalStateT)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager, Request(..), parseRequest, httpLbs, responseBody, Response, HttpException, RequestBody(..), responseTimeoutNone)
import Network.HTTP.Types.Header (hAuthorization, hContentType)
import Network.URI (URI)
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.Posix.Unistd (SysVar(..), getSysVar)
import System.Process (readCreateProcessWithExitCode, proc, CreateProcess(..))

import Text.Parsec hiding (try)           -- hide parsec's try, we'll qualify it
import qualified Text.Parsec as P
import Text.Parsec.Text (Parser)
import Data.Maybe

-- | Which HTTP-based LLM provider a backend targets.
data WebProvider = ProviderOpenAI | ProviderAnthropic | ProviderOllama
  deriving (Show, Eq, Generic)

-- | Errors that can occur when calling an LLM backend.
data LLMError
  = LLMHttpError T.Text        -- ^ HTTP or network-level failure
  | LLMParseError T.Text       -- ^ Could not decode the provider's response
  | LLMProcessError Int T.Text -- ^ CLI subprocess exited with a non-zero code
  | LLMArgMaxExceeded Int Int  -- ^ actualBytes argMaxBytes — execve arg list exceeded ARG_MAX (from gs/gets-context)
  deriving (Show, Eq, Generic)

-- | Transport + provider data for an LLM backend.
--
-- APIWeb: Manager, base URL, API key, model name, max tokens, provider
-- APICLI: executable path, model name, extra args inserted between the
--         fixed @-p --model M --dangerously-skip-permissions@ flags and
--         the prompt body (e.g. @["--debug"]@). Pass @[]@ for the
--         default invocation.
-- APIClosure: generic closure-based backend — the ask function is
--         supplied verbatim. Lets provider-specific modules
--         (e.g. "LLM.Provider.AnthropicOAuth") layer themselves ABOVE
--         'LLM.Provider' without creating an import cycle. Named
--         distinctly from 'APIMock' because, unlike a mock, this
--         constructor is intended for real backends that just happen
--         to live in a module 'LLM.Provider' can't import directly.
-- APIMock: pure function (for tests)
data LLMAPI
  = APIWeb Manager URI T.Text T.Text (Maybe Int) WebProvider
  | APICLI FilePath T.Text [String]
  | APIClosure ([ContentWithRole] -> IO (Either LLMError T.Text))
  | APIMock ([ContentWithRole] -> IO (Either LLMError T.Text))

-- | A named LLM backend (display name + transport details).
data LLMBackend = LLMBackend
  { _llmBackend_name :: T.Text  -- ^ Human-readable name, e.g. @\"openai\/gpt-4o\"@
  , _llmBackend_api  :: LLMAPI  -- ^ Transport and credentials
  }

-- | Environment for the 'LLMT' monad — holds the active backend.
data LLMEnv = LLMEnv
  { _llmEnv_backend :: LLMBackend
  , _llmEnv_maxContextChars :: Maybe Int
    -- ^ If Just n, context exceeding n chars is summarized before sending.
    -- Set this for CLI backends where ARG_MAX is a concern.
    -- Nothing = no limit (HTTP backends, mock backends).
  }

-- | Construct an LLMEnv with no context limit (backward compat).
mkLLMEnv :: LLMBackend -> LLMEnv
mkLLMEnv b = LLMEnv b Nothing

-- | Stateless LLM monad transformer.
newtype LLMT m a = LLMT { unLLMT :: ReaderT LLMEnv m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

-- | Stateful conversation monad: conversation history + LLM backend.
newtype ConvoT m a = ConvoT { unConvoT :: StateT ConversationHistory (ReaderT LLMEnv m) a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans ConvoT where
  lift = ConvoT . lift . lift

-- | Dispatch to the appropriate provider based on LLMAPI data.
askBackend :: LLMBackend -> [ContentWithRole] -> IO (Either LLMError T.Text)
askBackend backend msgs = case _llmBackend_api backend of
  APIWeb mgr url key model maxToks prov -> case prov of
    ProviderOpenAI    -> askOpenAI mgr (show url) key model maxToks Nothing msgs
    ProviderAnthropic -> askAnthropic mgr (show url) key model msgs
    ProviderOllama    -> askOllama mgr (show url) model msgs
  APICLI exec model extraArgs -> askCLI exec model extraArgs msgs
  APIClosure f         -> f msgs
  APIMock f            -> f msgs

-- | Send messages to the configured backend and return the raw text response.
askLLM :: MonadIO m => [ContentWithRole] -> LLMT m (Either LLMError T.Text)
askLLM msgs = LLMT $ do
  backend <- asks _llmEnv_backend
  liftIO $ askBackend backend msgs

-- | Convert T.Text to whatever Scrappy's `Html` type synonym resolves to in
-- the current scrappy-core version (String on some versions, Text on others).
-- Uses `IsString`'s `fromString` so the compiler picks the right concrete form
-- without a CPP conditional. Phase A cross-GHC compat.
toHtml :: T.Text -> Html
toHtml = fromString . T.unpack

-- | MERGE NOTE: this file used to carry a Stream-polymorphic reimplementation
-- of scrappy-json's parseJValue (parseJValueP) because scrappy-json's own
-- parseJValue was specialised to `Parsec String u JValue`. That duplication
-- has been removed as of TypifyDev/scrappy-json#2 — scrappy-json's parseJValue
-- is now Stream-polymorphic. The old implementation is commented out below
-- rather than deleted so blame + the design rationale remain visible.
{-
parseJValueP :: (Stream s m Char) => ParsecT s u m JValue
parseJValueP = P.try pObjectP
           <|> P.try pArrayP
           <|> P.try pStringP
           <|> P.try pNumberP
           <|> P.try pBoolP
           <|> pNullP
  where
    wsP :: (Stream s m Char) => ParsecT s u m ()
    wsP = skipMany (oneOf " \t\n\r")

    pObjectP = do
      _ <- char '{'
      wsP
      pairs <- pPairP `sepBy` P.try (wsP >> char ',' >> wsP)
      wsP
      _ <- char '}'
      pure $ JObject pairs

    pPairP = do
      key <- jsonStringBody
      wsP
      _ <- char ':'
      wsP
      val <- parseJValueP
      pure (key, val)

    pArrayP = do
      _ <- char '['
      wsP
      vals <- parseJValueP `sepBy` P.try (wsP >> char ',' >> wsP)
      wsP
      _ <- char ']'
      pure $ JArray vals

    pStringP = JString <$> jsonStringBody

    pNumberP = JNumber <$> numStr
      where
        numStr = do
          sign <- option "" (string "-")
          int' <- jsonInt
          frac <- option "" $ do
            d <- char '.'
            ds <- P.many1 digit
            pure (d : ds)
          ex <- option "" $ do
            e <- oneOf "eE"
            s <- option "" (string "+" <|> string "-")
            ds <- P.many1 digit
            pure (e : s ++ ds)
          pure $ sign ++ int' ++ frac ++ ex
        jsonInt = do
          d <- digit
          if d == '0'
            then pure "0"
            else (d :) <$> P.many digit

    pBoolP = (P.try (string "true")  >> pure (JBool True))
         <|> (string "false" >> pure (JBool False))

    pNullP = string "null" >> pure JNull
-}

-- | Try to extract a JSON value from LLM output text. Scrapes through prose
-- and code fences to find the first valid JSON object, then converts via 'FromJValue'.
-- Uses scrappy-json's Stream-polymorphic 'parseJValue'; works regardless of
-- which concrete type Scrappy's 'Html' resolves to in the current scrappy-core.
parseLLMJSON :: FromJValue a => T.Text -> Maybe a
parseLLMJSON txt = scrapeFirst' parseJValue (toHtml txt) >>= fromJValue

-- | Like 'askLLM' but parses the response as JSON via scrappy-json's 'FromJValue'.
-- Returns @Right Nothing@ when the LLM responds but no valid JSON is found.
askLLMJSON :: (MonadIO m, FromJValue a) => [ContentWithRole] -> LLMT m (Either LLMError (Maybe a))
askLLMJSON msgs = do
  result <- askLLM msgs
  pure $ case result of
    Left e -> Left e
    Right txt -> Right $ parseLLMJSON txt

-- | Like 'askLLMJSON' but uses a 'ReadLLM' Parsec parser instead of aeson's 'FromJSON'.
-- Extracts the first match from noisy LLM output (code fences, prose, etc.).
askLLMParsec :: (MonadIO m, ReadLLM a) => [ContentWithRole] -> LLMT m (Either LLMError (Maybe a))
askLLMParsec msgs = do
  result <- askLLM msgs
  pure $ case result of
    Left e -> Left e
    Right txt -> Right $ case search (toHtml txt) of
        Just (x:_) -> Just x
        _          -> Nothing

-- | Run a stateless LLM computation with the given environment.
runLLM :: LLMEnv -> LLMT m a -> m a
runLLM llmEnv (LLMT action) = runReaderT action llmEnv

-- | Run a stateful conversation, starting with empty history.
runConvo :: Monad m => LLMEnv -> ConvoT m a -> m a
runConvo llmEnv (ConvoT action) = runReaderT (evalStateT action []) llmEnv

-- ============================================================
-- Provider-specific HTTP/CLI implementations
-- ============================================================

-- | Updated askOpenAI that supports tools. Pass 'Nothing' for mTools to get
-- the old text-only behaviour.
askOpenAI :: Manager -> String -> T.Text -> T.Text -> Maybe Int -> Maybe [ToolDef] -> [ContentWithRole] -> IO (Either LLMError T.Text)
askOpenAI mgr url apiKey model maxTokens mTools msgs = do
  result <- askOpenAITools mgr url apiKey model maxTokens mTools (map toOpenAIMessage msgs)
  pure $ case result of
    Left e                    -> Left e
    Right (ToolResponseText t) -> Right t
    Right (ToolResponseCalls _) -> Left $ LLMHttpError "Model returned tool_calls but no tool executor was provided. Use askOpenAITools + runToolLoop."

{- MERGE NOTE: newSorted/gs/pre-merge-newSorted-dirty shipped the OLD text-only
   askOpenAI with its HTTP implementation inlined. HEAD's version above has
   been refactored to delegate to askOpenAITools (which supports both text and
   tool_calls responses) — the inlined HTTP implementation now lives in
   askOpenAITools. Old shape preserved below for the audit trail:

   askOpenAI :: Manager -> String -> T.Text -> T.Text -> Maybe Int -> [ContentWithRole] -> IO (Either LLMError T.Text)
   askOpenAI mgr url apiKey model maxTokens msgs = do
     req <- parseRequest url
     let headers =
           [ (hAuthorization, TE.encodeUtf8 $ "Bearer " <> T.strip apiKey)
           , (hContentType, "application/json")
           ]
         body = Aeson.object $
           [ "model"    Aeson..= model
           , "messages" Aeson..= msgs
           ] ++ maybe [] (\t -> ["max_tokens" Aeson..= t]) maxTokens
         req' = req { requestHeaders = headers, method = "POST", requestBody = RequestBodyLBS $ Aeson.encode body }
     (CE.try $ fmap responseBody $ httpLbs req' mgr) >>= \case
       Left (e :: HttpException) -> pure $ Left $ LLMHttpError $ T.pack $ show e
       Right resBody -> case Aeson.eitherDecode resBody :: Either String PromptResponse of
         Left e -> case Aeson.eitherDecode resBody :: Either String ErrorResponseOpenAI of
           Left ee -> pure . Left . LLMHttpError . T.pack $ e <> ee
           Right (ErrorResponseOpenAI (ErrorOpenAI msg _ _ _)) -> pure $ Left $ LLMHttpError msg
         Right r -> case choices r of
           [] -> pure $ Left $ LLMParseError "No choices"
           (c:_) -> pure . Right . _cwr_content . message $ c
-}

askAnthropic :: Manager -> String -> T.Text -> T.Text -> [ContentWithRole] -> IO (Either LLMError T.Text)
askAnthropic mgr url apiKey model msgs = do
  req <- parseRequest url
  let (sysMsgs, nonSysMsgs) = foldr partitionRole ([], []) msgs
      partitionRole m (sys, rest)
        | _cwr_role m == System = (m : sys, rest)
        | otherwise = (sys, m : rest)
      systemText = T.intercalate "\n" (map _cwr_content sysMsgs)
      body = Aeson.object $
        [ "model"      Aeson..= model
        , "max_tokens" Aeson..= (4096 :: Int)
        , "messages"   Aeson..= map toAnthropicMsg nonSysMsgs
        ] ++ [ "system" Aeson..= systemText | not (T.null systemText) ]
      toAnthropicMsg m = Aeson.object
        [ "role"    Aeson..= roleToText (_cwr_role m)
        , "content" Aeson..= _cwr_content m
        ]
      roleToText :: GPTRole -> T.Text
      roleToText System    = "user"
      roleToText User      = "user"
      roleToText Assistant = "assistant"
      headers =
        [ ("x-api-key", TE.encodeUtf8 apiKey)
        , ("anthropic-version", "2023-06-01")
        , ("content-type", "application/json")
        ]
      req' = req
        { method = "POST"
        , requestHeaders = headers
        , requestBody = RequestBodyLBS (Aeson.encode body)
        }
  (CE.try (httpLbs req' mgr) :: IO (Either HttpException (Response BL.ByteString))) >>= \case
    Left e -> pure $ Left $ LLMHttpError (T.pack $ show e)
    Right resp -> do
      let respBody = responseBody resp
      case Aeson.eitherDecode respBody :: Either String AnthropicResponse of
        Left e -> pure $ Left $ LLMParseError (T.pack e)
        Right ar -> case _anthropicResponse_content ar of
          (c:_) -> pure $ Right (_anthropicContent_text c)
          []    -> pure $ Left $ LLMParseError "Empty content in Anthropic response"

askOllama :: Manager -> String -> T.Text -> [ContentWithRole] -> IO (Either LLMError T.Text)
askOllama mgr url model msgs = do
  req <- parseRequest url
  let headers = [ (hContentType, "application/json") ]
      body = Aeson.object
        [ "model"    Aeson..= model
        , "messages" Aeson..= msgs
        , "stream"   Aeson..= False
        ]
      req' = req
        { requestHeaders = headers
        , method = "POST"
        , responseTimeout = responseTimeoutNone
        , requestBody = RequestBodyLBS $ Aeson.encode body
        }
  (CE.try $ fmap responseBody $ httpLbs req' mgr :: IO (Either HttpException BL.ByteString)) >>= \case
    Left e -> pure $ Left $ LLMHttpError $ T.pack $ show e
    Right resBody -> case Aeson.eitherDecode resBody :: Either String DeepSeekResponse of
      Left e -> pure . Left . LLMParseError . T.pack $ e
      Right a -> pure . Right . _cwr_content . _deepSeekResponse_message $ a

-- | Estimate execve payload size. Returns Left (actual, limit) if exceeded.
-- Pure helper — testable without spawning processes. (From gs/gets-context.)
checkArgMax :: Int -> [String] -> [(String, String)] -> Either (Int, Int) ()
checkArgMax argMaxBytes args envPairs =
  let argBytes = sum [ length a + 1 | a <- args ]       -- each arg + null terminator
      envBytes = sum [ length k + 1 + length v + 1 | (k, v) <- envPairs ]  -- "k=v\0"
      ptrOverhead = (length args + 1 + length envPairs + 1) * 8  -- argv/envp pointer arrays
      totalBytes = argBytes + envBytes + ptrOverhead
  in if totalBytes > argMaxBytes
    then Left (totalBytes, argMaxBytes)
    else Right ()

-- | Shell out to a Claude-style CLI. @extraArgs@ are inserted between
-- the fixed @-p --model M --dangerously-skip-permissions@ flags and
-- the prompt body — use them to pin an alternate model, add a debug
-- flag, etc., without touching this function.
askCLI :: FilePath -> T.Text -> [String] -> [ContentWithRole] -> IO (Either LLMError T.Text)
askCLI exec model extraArgs msgs = do
  let combinedPrompt = T.unpack $ T.intercalate "\n\n"
        [ _cwr_content m | m <- msgs ]
  curEnv <- getEnvironment
  -- MERGE NOTE: step-5 brought in a bare `args = [...]` block earlier that
  -- lacked extraArgs; it was unused and shadowed the real `args` below.
  -- Removed to resolve the unused-local-binds + name-shadowing -Werror pair.
  let cleanEnv = filter ((/= "CLAUDECODE") . fst) curEnv
      args = [ "-p"
             , "--model", T.unpack model
             , "--dangerously-skip-permissions"
             ]
          <> extraArgs
          <> [ combinedPrompt ]
  -- MERGE: ArgMax guard from gs/gets-context — bail out before execve if the
  -- argv/envp payload would exceed the kernel's ARG_MAX, so the caller sees
  -- LLMArgMaxExceeded instead of a bare E2BIG from the OS.
  argMax <- getSysVar ArgumentLimit
  case checkArgMax (fromIntegral argMax) (exec : args) cleanEnv of
    Left (actual, limit) -> pure $ Left $ LLMArgMaxExceeded actual limit
    Right () -> do
      let cp = (proc exec args) { env = Just cleanEnv }
      (CE.try $ readCreateProcessWithExitCode cp "" :: IO (Either CE.SomeException (ExitCode, String, String))) >>= \case
        Left e ->
          pure $ Left $ LLMProcessError 1 (T.pack $ show e)
        Right (ExitSuccess, out, _) ->
          pure $ Right (T.pack out)
        Right (ExitFailure code, _, err) ->
          pure $ Left $ LLMProcessError code (T.pack err)

-- ============================================================
-- Tool support (OpenAI / OpenRouter)
-- ============================================================

-- | What the model returned: either plain text or a request to run tools.
data ToolResponse
  = ToolResponseText T.Text
  | ToolResponseCalls [ToolUse]
  deriving (Show)

-- | Convert OpenAI ToolCall to our ToolUse (parse the JSON arguments).
toolCallToUse :: ToolCall -> Either String ToolUse
toolCallToUse tc = case Aeson.eitherDecode (BL.fromStrict $ TE.encodeUtf8 args) of
  Left e  -> Left $ "Failed to parse arguments for " <> T.unpack name <> ": " <> e
  Right v -> Right $ ToolUse cid name v
  where
    cid  = _toolCall_id tc
    name = _functionCall_name $ _toolCall_function tc
    args = _functionCall_arguments $ _toolCall_function tc

-- -- | Fallback parser for models that emit tool calls as pseudo-XML text
-- -- instead of the native OpenAI tool_calls field.
-- -- Handles: <function/name {"arg": "val"}>  or  <function/name {"arg": "val"}</function>
-- parseTextToolCalls :: T.Text -> Maybe [ToolUse]
-- parseTextToolCalls txt = case extractAll (T.strip txt) [] of
--   [] -> Nothing
--   us -> Just us
--   where
--     extractAll t acc
--       | T.null t = reverse acc
--       | otherwise = case T.breakOn "<function/" t of
--           (_, "") -> reverse acc
--           (_, rest) ->
--             let afterTag  = T.drop (T.length "<function/") rest
--                 (name, afterName) = T.break (== ' ') afterTag
--                 jsonStart = T.dropWhile (== ' ') afterName
--             in case extractJson jsonStart of
--                  Just (jsonRaw, afterJson) ->
--                    case Aeson.eitherDecode (BL.fromStrict $ TE.encodeUtf8 jsonRaw) of
--                      Right v ->
--                        let afterJson' = dropClosingTag afterJson
--                        in extractAll afterJson' (ToolUse ("text-" <> name) name v : acc)
--                      Left _ -> extractAll (skipTag afterJson) acc
--                  Nothing -> reverse acc

--     dropClosingTag = T.replace "</function>" "" . T.dropWhile (`elem` ['>', ' '])
--     skipTag s = T.drop 1 (T.dropWhile (/= '>') s)

--     -- Extract a balanced JSON object starting at the first '{'
--     extractJson s =
--       let s' = T.dropWhile (/= '{') s
--       in if T.null s'
--          then Nothing
--          else let (obj, rest) = matchBrace (T.drop 1 s') 1 ""
--               in Just ("{" <> obj, rest)

--     matchBrace :: T.Text -> Int -> T.Text -> (T.Text, T.Text)
--     matchBrace t depth acc
--       | T.null t = (acc, "")
--       | depth == 0 = (acc, t)
--       | otherwise =
--           let c = T.head t
--               rest = T.tail t
--           in case c of
--                '{' -> matchBrace rest (depth + 1) (acc <> "{")
--                '}' -> matchBrace rest (depth - 1) (acc <> "}")
--                _   -> matchBrace rest depth (acc <> T.singleton c)

-- | Fallback parser for models that emit tool calls as pseudo-XML text.
-- Handles multiple formats: <function/name {...}>, <function/name>{...}</function>
-- parseTextToolCalls :: T.Text -> Maybe [ToolUse]
-- parseTextToolCalls txt = case extractAll (T.strip txt) [] of
--   [] -> Nothing
--   us -> Just us
--   where
--     extractAll t acc
--       | T.null t = reverse acc
--       | otherwise = case T.breakOn "<function/" t of
--           (_, rest)
--             | T.null rest -> reverse acc
--             | otherwise ->
--                 let afterTag  = T.drop (T.length "<function/") rest
--                     -- Handle both: "bash {...}" and "bash>{...}"
--                     (name, afterName) = T.break (\c -> c == ' ' || c == '>') afterTag
--                     jsonStart = case T.uncons afterName of
--                       Just ('>', s) -> s
--                       Just (' ', s) -> T.dropWhile (== ' ') s
--                       _ -> afterName
--                 in case extractJson jsonStart of
--                      Just (jsonRaw, afterJson) ->
--                        case Aeson.eitherDecode (BL.fromStrict $ TE.encodeUtf8 jsonRaw) of
--                          Right v ->
--                            let afterJson' = dropClosingTag afterJson
--                            in extractAll afterJson' (ToolUse ("text-" <> name) name v : acc)
--                          Left _ -> extractAll (skipTag afterJson) acc
--                      Nothing -> extractAll (skipTag afterTag) acc

--     dropClosingTag = T.replace "</function>" "" . T.dropWhile (`elem` ['>', ' '])
--     skipTag s = T.drop 1 (T.dropWhile (/= '>') s)

--     extractJson s =
--       let s' = T.dropWhile (/= '{') s
--       in if T.null s'
--          then Nothing
--          else let (obj, rest) = matchBrace (T.drop 1 s') 1 ""
--               in Just ("{" <> obj, rest)

--     matchBrace :: T.Text -> Int -> T.Text -> (T.Text, T.Text)
--     matchBrace t depth acc
--       | T.null t = (acc, "")
--       | depth == 0 = (acc, t)
--       | otherwise =
--           let c = T.head t
--               rest = T.tail t
--           in case c of
--                '{' -> matchBrace rest (depth + 1) (acc <> "{")
--                '}' -> matchBrace rest (depth - 1) (acc <> "}")
--                _   -> matchBrace rest depth (acc <> T.singleton c)

-- | Fallback parser for models that emit tool calls as pseudo-XML text.
-- Handles all observed formats:
--   <function/name {"arg": "val"}>
--   <function/name>{"arg": "val"}</function>
--   <function/name{"arg": "val"}</function>
-- Extracts from anywhere in the text, skipping prose/reasoning.
parseTextToolCalls :: T.Text -> Maybe [ToolUse]
parseTextToolCalls txt = case parse toolCallsParser "" txt of
  Right us | not (null us) -> Just us
  _ -> Nothing

toolCallsParser :: Parser [ToolUse]
toolCallsParser = catMaybes <$> many toolCallOrSkip <* eof
  where
    toolCallOrSkip = P.try (Just <$> toolCallParser) <|> (anyChar *> pure Nothing)

toolCallParser :: Parser ToolUse
toolCallParser = do
  _ <- string "<function/"
  name <- many1 (noneOf " >{")
  -- After name: might be >, space, or directly {
  _ <- optional (oneOf " >")
  jsonStr <- balancedBraces
  -- Optionally consume closing tag variants
  _ <- optional (string "</function>")
  _ <- optional (char '>')
  case Aeson.eitherDecode (BL.fromStrict $ TE.encodeUtf8 $ T.pack jsonStr) of
    Right v -> pure $ ToolUse ("text-" <> T.pack name) (T.pack name) v
    Left e  -> parserFail $ "Invalid JSON in tool call: " <> e

-- | Parse a balanced {} object, including the opening brace.
balancedBraces :: Parser String
balancedBraces = do
  _ <- char '{'
  chars <- go 1 ""
  pure $ '{' : reverse chars
  where
    go :: Int -> String -> Parser String
    go 0 acc = pure acc
    go n acc = do
      c <- anyChar
      case c of
        '{' -> go (n + 1) (c : acc)
        '}' -> go (n - 1) (c : acc)
        _   -> go n     (c : acc)


-- | Low-level: send messages to OpenAI/OpenRouter and return either text or tool calls.
askOpenAITools :: Manager -> String -> T.Text -> T.Text -> Maybe Int -> Maybe [ToolDef] -> [OpenAIMessage] -> IO (Either LLMError ToolResponse)
askOpenAITools mgr url apiKey model maxTokens mTools msgs = do
  req <- parseRequest url
  let headers =
        [ (hAuthorization, TE.encodeUtf8 $ "Bearer " <> T.strip apiKey)
        , (hContentType, "application/json")
        ]
      body = GPTRequestBody
        { _gptRequest_model       = model
        , _gptRequest_max_tokens  = maxTokens
        , _gptRequest_messages    = msgs
        , _gptRequest_tools       = mTools
        , _gptRequest_tool_choice = case mTools of
            Just ts | not (null ts) -> Just "auto"
            _                       -> Nothing
        }
      req' = req
        { requestHeaders = headers
        , method         = "POST"
        , requestBody    = RequestBodyLBS $ Aeson.encode body
        }
  (CE.try $ fmap responseBody $ httpLbs req' mgr) >>= \case
    Left (e :: HttpException) -> pure $ Left $ LLMHttpError $ T.pack $ show e
    Right resBody -> do
      BL.writeFile "/tmp/openrouter-last.json" resBody
      case Aeson.eitherDecode resBody :: Either String OpenAIChatResponse of
        Left e -> case Aeson.eitherDecode resBody :: Either String ErrorResponseOpenAI of
          Left ee -> pure . Left . LLMHttpError . T.pack $ e <> ee
          Right (ErrorResponseOpenAI (ErrorOpenAI msg _ _ _)) ->
            pure $ Left $ LLMHttpError msg
        Right r -> case _oacr_choices r of
          [] -> pure $ Left $ LLMParseError "No choices"
          (c:_) -> let msg = _oac_message c in
            case _oam_tool_calls msg of
              Just tcs | not (null tcs) -> case traverse toolCallToUse tcs of
                Left e   -> pure $ Left $ LLMParseError $ T.pack e
                Right us -> pure $ Right $ ToolResponseCalls us
              _ -> case _oam_content msg of
                Just txt -> case parseTextToolCalls txt of
                  Just us  -> pure $ Right $ ToolResponseCalls us
                  Nothing  -> pure $ Right $ ToolResponseText txt
                Nothing  -> pure $ Right $ ToolResponseText ""

-- | Run the full tool loop with a max-round safety cap.
runToolLoop :: Manager -> String -> T.Text -> T.Text -> Maybe Int -> Int -> [ToolDef] -> ([ToolUse] -> IO [ToolResult]) -> [ContentWithRole] -> IO (Either LLMError T.Text)
runToolLoop mgr url apiKey model maxTokens maxRounds tools execToolF initialMsgs =
  go 0 [] (map toOpenAIMessage initialMsgs)
  where
    go n acc history
      | n >= maxRounds =
          pure $ Right $ T.unlines $
            "Reached max tool rounds. Here's what I found:" : reverse acc
      | otherwise = do
          result <- askOpenAITools mgr url apiKey model maxTokens (Just tools) history
          case result of
            Left e -> pure $ Left e
            Right (ToolResponseText txt) -> pure $ Right txt
            Right (ToolResponseCalls uses) -> do
              results <- execToolF uses
              let outputs = map _toolResult_content results
                  resultMsgs = map toolResultToMsg results
                  assistantMsg = OpenAIMessage
                    { _oam_role         = "assistant"
                    , _oam_content      = Nothing
                    , _oam_tool_calls   = Just $ map useToToolCall uses
                    , _oam_tool_call_id = Nothing
                    }
              go (n + 1) (outputs ++ acc) (history ++ [assistantMsg] ++ resultMsgs)

    useToToolCall :: ToolUse -> ToolCall
    useToToolCall (ToolUse cid name inputVal) = ToolCall
      { _toolCall_id       = cid
      , _toolCall_type     = "function"
      , _toolCall_function = FunctionCall
          { _functionCall_name      = name
          , _functionCall_arguments = TE.decodeUtf8 $ BL.toStrict $ Aeson.encode inputVal
          }
      }
-- MERGE NOTE: gs/gets-context ended its askCLI tail here with the ArgMax
-- guard wrapping readCreateProcessWithExitCode. That guard has been INJECTED
-- into HEAD's askCLI body above (see "MERGE: ArgMax guard from gs/gets-context"),
-- so this duplicate tail from gets-context is omitted — the guard is live in
-- the authoritative askCLI definition rather than preserved as dead code.
-- newSorted/gs/pre-merge-newSorted-dirty also shipped a third duplicate of
-- this tail (inline flags array, no extraArgs, qualified CE.CE.SomeException);
-- same reasoning applies — dropped here, the HEAD body above is authoritative.
