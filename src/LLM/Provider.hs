{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLM.Provider
  ( WebProvider(..)
  , LLMAPI(..)
  , LLMBackend(..)
  , LLMEnv(..)
  , LLMError(..)
  , LLMT(..)
  , ConvoT(..)
  , askBackend
  , askLLM
  , askLLMJSON
  , askLLMParsec
  , parseLLMJSON
  , runLLM
  , runConvo
  ) where

import LLM.Types
import LLM.ReadLLM (ReadLLM(..))
import Scrappy.JSON.Value (FromJValue, fromJValue, parseJValue)
import Scrappy.Scrape (scrapeFirst')

import Control.Exception as CE
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Trans.State (StateT, evalStateT)
import Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager, Request(..), parseRequest, httpLbs, responseBody, Response, HttpException, RequestBody(..), responseTimeoutNone)
import Network.HTTP.Types.Header (hAuthorization, hContentType)
import Network.URI (URI)
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.Process (readCreateProcessWithExitCode, proc, CreateProcess(..))

data WebProvider = ProviderOpenAI | ProviderAnthropic | ProviderOllama
  deriving (Show, Eq, Generic)

data LLMError
  = LLMHttpError T.Text
  | LLMParseError T.Text
  | LLMProcessError Int T.Text
  deriving (Show, Eq, Generic)

-- | Transport + provider data for an LLM backend.
--
-- APIWeb: Manager, base URL, API key, model name, max tokens, provider
-- APICLI: executable path, model name
-- APIMock: pure function (for tests)
data LLMAPI
  = APIWeb Manager URI T.Text T.Text (Maybe Int) WebProvider
  | APICLI FilePath T.Text
  | APIMock ([ContentWithRole] -> IO (Either LLMError T.Text))

data LLMBackend = LLMBackend
  { _llmBackend_name :: T.Text
  , _llmBackend_api  :: LLMAPI
  }

data LLMEnv = LLMEnv
  { _llmEnv_backend :: LLMBackend
  }

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
    ProviderOpenAI    -> askOpenAI mgr (show url) key model maxToks msgs
    ProviderAnthropic -> askAnthropic mgr (show url) key model msgs
    ProviderOllama    -> askOllama mgr (show url) model msgs
  APICLI exec model   -> askCLI exec model msgs
  APIMock f            -> f msgs

askLLM :: MonadIO m => [ContentWithRole] -> LLMT m (Either LLMError T.Text)
askLLM msgs = LLMT $ do
  backend <- asks _llmEnv_backend
  liftIO $ askBackend backend msgs

parseLLMJSON :: FromJValue a => T.Text -> Maybe a
parseLLMJSON txt = scrapeFirst' parseJValue (T.unpack txt) >>= fromJValue

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
    Right txt -> Right $ case search (T.unpack txt) of
        Just (x:_) -> Just x
        _          -> Nothing

runLLM :: LLMEnv -> LLMT m a -> m a
runLLM env (LLMT action) = runReaderT action env

runConvo :: Monad m => LLMEnv -> ConvoT m a -> m a
runConvo env (ConvoT action) = runReaderT (evalStateT action []) env

-- ============================================================
-- Provider-specific HTTP/CLI implementations
-- ============================================================

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
      req' = req
        { requestHeaders = headers
        , method = "POST"
        , requestBody = RequestBodyLBS $ Aeson.encode body
        }
  (CE.try $ fmap responseBody $ httpLbs req' mgr) >>= \case
    Left (e :: HttpException) -> pure $ Left $ LLMHttpError $ T.pack $ show e
    Right resBody -> case eitherDecode resBody :: Either String PromptResponse of
      Left e -> case eitherDecode resBody :: Either String ErrorResponseOpenAI of
        Left ee -> pure . Left . LLMHttpError . T.pack $ e <> ee
        Right (ErrorResponseOpenAI (ErrorOpenAI msg _ _ _)) ->
          pure $ Left $ LLMHttpError msg
      Right r -> case choices r of
        [] -> pure $ Left $ LLMParseError "No choices"
        (c:_) -> pure . Right . _cwr_content . message $ c

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
  (CE.try (httpLbs req' mgr) :: IO (Either HttpException (Response LBS.ByteString))) >>= \case
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
  (CE.try $ fmap responseBody $ httpLbs req' mgr :: IO (Either HttpException LBS.ByteString)) >>= \case
    Left e -> pure $ Left $ LLMHttpError $ T.pack $ show e
    Right resBody -> case eitherDecode resBody :: Either String DeepSeekResponse of
      Left e -> pure . Left . LLMParseError . T.pack $ e
      Right a -> pure . Right . _cwr_content . _deepSeekResponse_message $ a

askCLI :: FilePath -> T.Text -> [ContentWithRole] -> IO (Either LLMError T.Text)
askCLI exec model msgs = do
  let combinedPrompt = T.unpack $ T.intercalate "\n\n"
        [ _cwr_content m | m <- msgs ]
  curEnv <- getEnvironment
  let cleanEnv = filter ((/= "CLAUDECODE") . fst) curEnv
      cp = (proc exec
              [ "-p"
              , "--model", T.unpack model
              , "--dangerously-skip-permissions"
              , combinedPrompt
              ]) { env = Just cleanEnv }
  (CE.try $ readCreateProcessWithExitCode cp "" :: IO (Either SomeException (ExitCode, String, String))) >>= \case
    Left e ->
      pure $ Left $ LLMProcessError 1 (T.pack $ show e)
    Right (ExitSuccess, out, _) ->
      pure $ Right (T.pack out)
    Right (ExitFailure code, _, err) ->
      pure $ Left $ LLMProcessError code (T.pack err)
