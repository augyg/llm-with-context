{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | High-level functions for calling OpenAI (GPT) and DeepSeek\/Ollama models.
--
-- Two backends are supported:
--
-- * __OpenAI__ — 'askGPT', 'askGPTTyped', 'askGPTJSON', 'askGPTWithContext',
--   'askGPTWithContextTyped'.  Requires an @'APIKey' \'OpenAI@ and an
--   @http-client@ 'Manager'.
--
-- * __DeepSeek \/ Ollama__ — 'askDeepSeek', 'askDeepSeekWithContext'.
--   Talks to a local Ollama instance on @localhost:11434@.
--
-- Both backends support stateful multi-turn conversations via 'MonadGPT' and
-- 'MonadDeepSeek', with pluggable context-selection through 'RelevantContext'
-- and 'RelevantContextDS'.
module LLM.LLM
  ( -- * OpenAI (GPT) direct calls
    askGPT
  , askGPTJSON
  , askGPTTyped
    -- * OpenAI with conversation context
  , askGPTWithContext
  , askGPTWithContextTyped
    -- * DeepSeek \/ Ollama
  , askDeepSeek
  , askDeepSeekWithContext
    -- * Provider-agnostic conversation layer
  , askWithContext
  , askJSONWithContext
    -- * Context selection
  , getRelevantCtx
  , getRelevantCtxDeepSeek
    -- * History rendering
  , renderHistory
    -- * DeepSeek response parsing
  , toThoughtResponse
  , codeBlock
    -- * Helpers
  , mkDSPrompt
  , gptReturnType
  , tshow
  , escapeText
  , escape
  , gptModel
  , getRelevant
  , TokenLimit
  ) where

import LLM.Types
import LLM.Provider (askLLM, LLMT(..), LLMError(..), ConvoT(..), parseLLMJSON)

import Scrappy.Elem as S hiding (Tag)
import Scrappy.JSON.Value (FromJValue)

import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Types.Header

import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Control.Exception as CE
import Data.Bifunctor
import Data.Aeson as Aeson
import Text.Parsec as Psc
import Data.Typeable
import Data.Default
import Text.Read (readEither)
import Data.Maybe (catMaybes)
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as LBS

-- | 'show' a value directly to 'T.Text'.
tshow :: Show a => a -> T.Text
tshow = T.pack . show



-- | Build a 'DeepSeekRequestBody' from a model size and message list,
-- using 'Default' for all other fields.
mkDSPrompt :: DeepSeekModel -> [ContentWithRole] -> DeepSeekRequestBody
mkDSPrompt dsModel cwrs = def
  { _deepSeekRequest_model = dsModel
  , _deepSeekRequest_messages = cwrs
  }
  


-- | Example 'RelevantContext': keep the last 10 items whose tag starts with @\"html\"@.
getRelevant :: RelevantContext
getRelevant = LastNRelevant 10 $ \(Tag t) -> T.isPrefixOf "html" t

-- | Render the full conversation history into a single 'Assistant' message
-- suitable for injecting as context before a new prompt.
renderHistory :: ConversationHistory -> ContentWithRole
renderHistory = cwr Assistant . ((<>) "Our conversation history so far:") . T.intercalate "\n" . fmap renderItem
  where
    renderItem (ConvoQuery _ (ConvoQuestion q) (ConvoAnswer a)) =
      "Me: " <> (T.decodeUtf8 . LBS.toStrict . Aeson.encode) q <> "\n" <> "Assistant: " <> a

-- | Like 'renderHistory' but lets the caller choose which role the rendered
-- history is attached to. OpenAI/DeepSeek inject history as an 'Assistant'
-- turn; Anthropic requires the first message to be a 'User' turn, so the
-- Anthropic provider injects history as a 'System' turn instead (which
-- 'LLM.Provider.Anthropic.splitSystem' hoists into the top-level "system"
-- field).
renderHistoryWithRole :: GPTRole -> ConversationHistory -> ContentWithRole
renderHistoryWithRole role =
  cwr role . ((<>) "Our conversation history so far:") . T.intercalate "\n" . fmap renderItem
  where
    renderItem (GPTQuery _ (GPTQuestion q) (GPTAnswer a)) =
      "Me: " <> (T.decodeUtf8 . LBS.toStrict . Aeson.encode) q <> "\n" <> "ChatGPT: " <> a




-- | Select conversation history items matching a 'RelevantContext' strategy.
getRelevantCtx :: Monad m => RelevantContext -> StateT ConversationHistory m ConversationHistory
getRelevantCtx = \case
  LastN n -> gets (take n)
  Relevants tags -> gets (flip finds tags)
  LastNRelevant n anonF -> gets (\x ->
                               take n
                               . filter (anonF . _convoQuery_tag) $ x
                            )
  where
    finds hist tags =
      catMaybes $ fmap (\t -> L.find (\h -> t == _convoQuery_tag h) hist) tags

-- | Select DeepSeek conversation history items matching a 'RelevantContextDS' strategy.
getRelevantCtxDeepSeek :: MonadIO m => RelevantContextDS -> MonadDeepSeek m ConversationHistoryDeepSeek
getRelevantCtxDeepSeek = \case
  LastN_DS n -> gets (take n)
  Relevants_DS tags -> gets (flip finds tags)
  LastNRelevant_DS n anonF -> gets (\historyTotal ->
                                  take n
                                  . filter (\(historyItem :: (TagDS, [ContentWithRole])) ->
                                              anonF . fst $ historyItem
                                           ) $ historyTotal
                               )
  where
    finds hist tags =
      catMaybes $ fmap (\t -> L.find (\h -> t == fst h) hist) tags


-- testctx :: MonadIO m => MonadGPT m ()
-- testctx = do
--   mgr <- liftIO $ newManager tlsManagerSettings
--   k <- liftIO $ fmap T.pack $ readFile "config/backend/gptAPIKey"
--   r1 <- askGPTWithContext k mgr (LastN 10) (Tag "Name", GPTQuestion [cwr User "my name is galen"])
--   r2 <- askGPTWithContext k mgr (LastN 10) (Tag "Hey", GPTQuestion [cwr User "please tell me what my name is"])
--   r3 :: Either GPTError (GPTAnswer Int) <- askGPTWithContextTyped k mgr (LastN 10) (Tag "Hey", GPTQuestion [cwr User "how many letters in my name"])
--   r4 :: Either GPTError (GPTAnswer Int) <- askGPTWithContextTyped k mgr (Relevants [Tag "Name"]) (Tag "Hey", GPTQuestion [cwr User "how many letters in my name"])
--   r5 :: Either GPTError (GPTAnswer Int) <- askGPTWithContextTyped k mgr (Relevants []) (Tag "Hey", GPTQuestion [cwr User "how many letters in my name"])
--   r6 :: Either GPTError (GPTAnswer Int) <- askGPTWithContextTyped k mgr (LastN 0) (Tag "Hey", GPTQuestion [cwr User "how many letters in my name"])
--   liftIO $ print r1
--   liftIO $ print r2
--   liftIO $ print r3
--   liftIO $ print r4
--   liftIO $ print r5
--   liftIO $ print r6
--   pure ()

-- | Ask GPT with conversation context and parse the response into a typed
-- Haskell value via 'Read'. The type to parse into is inferred from the
-- call site. Appends a system message instructing the model to return
-- only the requested Haskell type.
askGPTWithContextTyped
  :: forall m a.
  ( Typeable a
  , Read a
  , MonadIO m
  )
  => APIKey 'OpenAI
  -> Manager
  -> TokenLimit
  -> RelevantContext
  -> (Tag, ConvoQuestion)
  -> ConvoT m (Either ConvoError (ConvoAnswer a))
askGPTWithContextTyped key mgr tokenLimit relCtx (thisTag, ConvoQuestion contents) = ConvoT $ do
  let typeProxy = Proxy :: Proxy a
  let returnT = gptReturnType typeProxy
  let
    readEitherText :: T.Text -> Either T.Text a
    readEitherText = first T.pack . readEither2 . T.unpack
      where
        readEither2 x = case readEither x of
          Right a -> Right a
          Left _ -> case readEither $ "\"" <> x <> "\"" of
            Right a -> Right a
            Left _ -> readEither $ "\"" <> (T.unpack $ escapeText $ T.pack x) <> "\""

  ctx <- renderHistory <$> getRelevantCtx relCtx
  askGPT key mgr gptModel tokenLimit (ctx : contents <> returnT) >>= \case
    Left e -> pure . Left . ConvoError $ e
    Right txt -> case readEitherText txt of
      Left e -> pure . Left . ConvoError $
        e <> "When reading return type: (x :: "  <> (T.pack . show $ typeRep proxy ) <> ") from base response: " <> txt
        <> "From Prompt: "
        <> (T.pack $ show (ctx : contents <> returnT))

      Right typed -> do
        let new = ConvoQuery thisTag (ConvoQuestion contents) (ConvoAnswer txt)
        modify ((:) new)
        pure . Right . ConvoAnswer $ typed

-- | Ask GPT with conversation context injected. Retrieves relevant history
-- via 'getRelevantCtx', prepends it to the prompt, and stores the Q&A pair
-- in state on success.
askGPTWithContext
  :: MonadIO m
  => APIKey 'OpenAI
  -> Manager
  -> TokenLimit
  -> RelevantContext
  -> (Tag, ConvoQuestion)
  -> ConvoT m (Either ConvoError (ConvoAnswer T.Text))
askGPTWithContext key mgr maxTokens relCtx (thisTag, ConvoQuestion contents) = ConvoT $ do
  histItems <- getRelevantCtx relCtx
  askGPT key mgr gptModel maxTokens (renderHistory histItems : contents) >>= \case
    Left e -> pure $ Left $ ConvoError e
    Right answer -> do
      let new = ConvoQuery thisTag (ConvoQuestion contents) (ConvoAnswer answer)
      modify ((:) new)
      pure $ Right $ ConvoAnswer answer

-- | Transport-agnostic version of the "with context" loop, parameterised over
-- how history is injected and which stateless prim does the request. This lets
-- the OpenAI and Anthropic servant providers reuse the exact same StateT
-- bookkeeping without touching 'askGPTWithContext'. @injectHistory@ turns the
-- recalled turns into prompt messages (an 'Assistant' turn for OpenAI/DeepSeek,
-- a 'System' turn for Anthropic); @prim@ is the provider's stateless ask.
askWithContextBy
  :: MonadIO m
  => (ConversationHistory -> [ContentWithRole])
  -> (TokenLimit -> [ContentWithRole] -> MonadGPT m (Either T.Text T.Text))
  -> TokenLimit
  -> RelevantContext
  -> (Tag, GPTQuestion)
  -> MonadGPT m (Either GPTError (GPTAnswer T.Text))
askWithContextBy injectHistory prim maxTokens relCtx (thisTag, GPTQuestion contents) = do
  histItems <- getRelevantCtx relCtx
  prim maxTokens (injectHistory histItems <> contents) >>= \case
    Left e -> pure $ Left $ GPTError e
    Right answer -> do
      let new = GPTQuery thisTag (GPTQuestion contents) (GPTAnswer answer)
      modify ((:) new)
      pure $ Right $ GPTAnswer answer

-- | Transport-agnostic version of 'askGPTWithContextTyped', parameterised over
-- the stateless prim. The typed-decode logic lives here (rather than in a
-- provider module) so it keeps the same imports and behaviour as
-- 'askGPTWithContextTyped'. History is injected as an 'Assistant' turn, as the
-- OpenAI-style typed flow expects.
askGPTWithContextTypedBy
  :: forall m a.
  ( Typeable a
  , Read a
  , MonadIO m
  )
  => (TokenLimit -> [ContentWithRole] -> MonadGPT m (Either T.Text T.Text))
  -> TokenLimit
  -> RelevantContext
  -> (Tag, GPTQuestion)
  -> MonadGPT m (Either GPTError (GPTAnswer a))
askGPTWithContextTypedBy prim tokenLimit relCtx (thisTag, GPTQuestion contents) = do
  let typeProxy = Proxy :: Proxy a
  let returnT = gptReturnType typeProxy
  let
    readEitherText :: T.Text -> Either T.Text a
    readEitherText = first T.pack . readEither2 . T.unpack
      where
        readEither2 x = case readEither x of
          Right a -> Right a
          Left _ -> case readEither $ "\"" <> x <> "\"" of
            Right a -> Right a
            Left _ -> readEither $ "\"" <> (T.unpack $ escapeText $ T.pack x) <> "\""

  ctx <- renderHistory <$> getRelevantCtx relCtx
  prim tokenLimit (ctx : contents <> returnT) >>= \case
    Left e -> pure . Left . GPTError $ e
    Right txt -> case readEitherText txt of
      Left e -> pure . Left . GPTError $
        e <> "When reading return type: (x :: "  <> (T.pack . show $ typeRep typeProxy ) <> ") from base response: " <> txt
        <> "From Prompt: "
        <> (T.pack $ show (ctx : contents <> returnT))

      Right typed -> do
        let new = GPTQuery thisTag (GPTQuestion contents) (GPTAnswer txt)
        modify ((:) new)
        pure . Right . GPTAnswer $ typed





-- Script flow:
  -- give Title and unique story to focus on
  --   + give instructions for any script (factuality, stats-heavy, clips direction etc.)
  --   + category specific instructions
  -- Instruct DeepSeek to return a high level outline of what the script should be as JSON
  -- JSON:
  -- { "title": ::String
  -- , "sections" : [ { "section_title": <String>
  --                  , "suggested_word_count": Int
  --                  , "predicted_time_seconds": Int
  --                  , "section_overview": <String> 
  --                  }
  -- ,              ]
  -- }                        


-- runStateAction :: MonadIO m => Manager -> DeepSeekModel -> [ContentWithRole] -> MonadDeepSeek m (Either GPTError DeepSeekAnswer)
-- runStateAction mgr modelChoice cwrs = askDeepSeekWithContext mgr modelChoice (LastN_DS 1000) (TagDS "sometag" False, DeepSeekQuestion cwrs)


-- | Ask DeepSeek with conversation context. Concatenates relevant history
-- messages before the new prompt and stores both the question and answer
-- in state on success.
askDeepSeekWithContext
  :: MonadIO m
  => Manager
  -> DeepSeekModel
  -> RelevantContextDS
  -> (TagDS, DeepSeekQuestion)
  -> MonadDeepSeek m (Either ConvoError DeepSeekAnswer)
askDeepSeekWithContext mgr modelDS relCtx (thisTag, DeepSeekQuestion contents) = do

  histItems <- getRelevantCtxDeepSeek relCtx
  let
    historyAtNow = (mconcat $ reverse $ fmap snd histItems)
    fullCWRs = historyAtNow <> contents -- [1,2] <> [3] --> [1,2,3]

  deepSeekResult <- askDeepSeek mgr modelDS $ fullCWRs

  case deepSeekResult of
    Left e -> pure $ Left . ConvoError $ e
    Right res -> do
      let newAnswer = _deepSeekResponse_message res
      modify (\state_ ->
                let question = (thisTag, contents)
                    answer = (TagDS (unTagDS thisTag) True, [newAnswer])
                in
                  answer : question : state_
             )
      pure $ Right . ConvoAnswer $ newAnswer

-- | Ask GPT and parse the response into a typed Haskell value (no context).
-- Appends a system message telling the model to return only the Haskell type @a@.
--
-- TODO: Configure temperature for less variability.
askGPTTyped
  :: forall m a.
  ( MonadIO m
  , Typeable a
  , Read a
  )
  => APIKey 'OpenAI
  -> Manager
  -> TokenLimit
  -> ConvoQuestion
  -> m (Either ConvoError (ConvoAnswer a))
askGPTTyped apiKey mgr maxTokens (ConvoQuestion prompt) = do
  let typeProxy = Proxy :: Proxy a
  let returnT = gptReturnType typeProxy -- "Please only respond with nothing but the haskell type Map Int Int"
  let
    readEitherText :: T.Text -> Either T.Text a
    readEitherText = first T.pack . readEither2 . T.unpack
      where readEither2 x = case readEither x of
              Right a -> Right a
              Left _ -> readEither $ "\"" <> escape x <> "\""

  r <- askGPT apiKey mgr gptModel maxTokens $ prompt <> returnT
  pure . bimap ConvoError ConvoAnswer $ readEitherText =<< r

-- | Escape double-quotes and backslashes in 'T.Text' for safe embedding in JSON strings.
escapeText :: T.Text -> T.Text
escapeText = T.concatMap escapeChar
  where
    escapeChar '\"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar c    = T.singleton c


-- | Escape regex-special characters in a 'String'.
escape :: String -> String
escape = concatMap esc
    where
        escchars :: String
        escchars = "$\\^.*~[]"
        esc c   | c `elem` escchars = ['\\',c]
                | otherwise         = [c]

-- | Generate a system message instructing the model to return only a value
-- of Haskell type @a@. Returns @[]@ for 'T.Text' and 'String' (no constraint needed).
gptReturnType :: forall a. Typeable a => Proxy a -> [ContentWithRole]
gptReturnType typeProxy =
  let
    typeInfo = T.pack $ show (typeRep typeProxy)
  in
    if typeInfo == "Text" || typeInfo == "String"
    then []
    else [ cwr System $ "In responding to the above question, give me only the Haskell type:" <> typeInfo <> " and nothing else in your response: Format should be parsable as the Haskell type:" <> typeInfo ]

-- | Pure core of 'getRelevantCtx': the history-selection logic without the
-- 'StateT' wrapper, so the effect interpreters (which carry history in an
-- effectful 'State') can reuse the exact same selection rules.
selectRelevant :: RelevantContext -> ConversationHistory -> ConversationHistory
selectRelevant = \case
  LastN n -> take n
  Relevants tags -> \hist -> catMaybes $ fmap (\t -> L.find (\h -> t == _gptQuery_tag h) hist) tags
  LastNRelevant n anonF -> take n . filter (anonF . _gptQuery_tag)

-- | The three-tier read used by the typed variants, factored out of
-- 'askGPTWithContextTyped' so the plain typed prim ('askTypedBy') and the
-- effect interpreters share one decoder. Tries the raw text, then quoted, then
-- escaped-and-quoted.
readTypedAnswer :: forall a. Read a => T.Text -> Either T.Text a
readTypedAnswer = first T.pack . readEither2 . T.unpack
  where
    readEither2 x = case readEither x of
      Right a -> Right a
      Left _ -> case readEither $ "\"" <> x <> "\"" of
        Right a -> Right a
        Left _ -> readEither $ "\"" <> (T.unpack $ escapeText $ T.pack x) <> "\""

-- | Transport-agnostic typed prim: append the return-type instruction, run the
-- supplied stateless text prim, and decode the answer to @a@. Mirrors
-- 'askGPTTyped' but parameterised over the prim so any provider can reuse it.
-- The token cap is decided by the prim's own config (Reader), so it is not a
-- parameter here. 'Monad' rather than 'MonadIO' since this touches no IO — that
-- also lets the pure mock interpreter reuse it.
askTypedBy
  :: forall m a. (Monad m, Typeable a, Read a)
  => ([ContentWithRole] -> m (Either T.Text T.Text))
  -> [ContentWithRole]
  -> m (Either GPTError (GPTAnswer a))
askTypedBy prim contents = do
  let returnT = gptReturnType (Proxy :: Proxy a)
  r <- prim (contents <> returnT)
  pure . bimap GPTError GPTAnswer $ readTypedAnswer =<< r

type TokenLimit = Maybe Int

-- | Low-level OpenAI chat-completion call. Sends messages to
-- @\/v1\/chat\/completions@ and returns the first choice's content or an error.
askGPT :: MonadIO m => APIKey 'OpenAI -> Manager -> T.Text -> TokenLimit -> [ContentWithRole] -> m (Either T.Text T.Text)
askGPT apiKey mgr modelName maxTokens contents = liftIO $ do
  putStrLn "askGPT"
  let url = "https://api.openai.com/v1/chat/completions"
  req <- parseRequest url
  let headers = [ (hAuthorization, "Bearer " <> (T.unpack . T.strip . unAPIKey $ apiKey))
                , (hContentType, "application/json")
                ]
  let promptLen = maybe [] (\_len -> [cwr System $ "Please limit response to " <> (T.pack $ show (50 :: Integer)) <> " tokens"]) maxTokens
  -- We add 50 to limit because as the request gets larger GPT is worse at knowing when to stop
  -- This should not affect shorter responses
  let prompt = GPTRequestBody
        { _gptRequest_model = modelName
        , _gptRequest_max_tokens = (+ 50) <$> maxTokens
        , _gptRequest_messages = promptLen <> contents
        }
  let req' = req { requestHeaders = (fmap . fmap) (T.encodeUtf8 . T.pack) headers
                 , method = "POST"
                 , requestBody = RequestBodyLBS $ Aeson.encode prompt --txt
                 }
  liftIO $ print $ Aeson.encode prompt
  (CE.try $ fmap responseBody $ httpLbs req' mgr) >>= \case
    Left (e :: HttpException) -> pure $ Left $ tshow e
    Right resBody -> case eitherDecode resBody :: Either String PromptResponse of
      Left e -> case eitherDecode resBody :: Either String ErrorResponseOpenAI of
        Left ee -> pure . Left . T.pack $ e <> ee
        Right (ErrorResponseOpenAI (ErrorOpenAI msg _ _ _)) -> do
          liftIO $ print $ "Error with OpenAI: " <> msg
          pure $ Left "Unknown AI Error"
      Right r -> case choices r of
        [] -> pure $ Left "No choices"
        (c:_) -> pure . Right . _cwr_content . message $ c


-- curl -X POST http://localhost:11434/api/generate      -d '{
--            "model": "deepseek-r1:1.5b",
--            "prompt": "Write a haiku about the moon.",
--            "stream": true
--          }'      -H "Content-Type: application/json"



-- | Parse a DeepSeek-R1 response that contains @\<think\>...\<\/think\>@ reasoning
-- into a structured 'ThoughtResponse' with separate think and answer sections.
toThoughtResponse :: ContentWithRole -> Either Psc.ParseError ThoughtResponse
toThoughtResponse r =
  let
    contentSrc :: T.Text
    contentSrc = _cwr_content $ r
    p = do
      thinkTag <- S.el "think" []
      rest <- Psc.many Psc.anyChar
      pure $ ThoughtResponse (T.lines . T.pack $ S.innerText' thinkTag) (T.lines . T.pack $ rest)
  in
    Psc.parse p "deepseek response" contentSrc

-- import Text.Parsec as Psc

-- | Parsec parser for a fenced code block (@\`\`\`lang ... \`\`\`@).
-- Returns @(language, code)@.
codeBlock :: Psc.Stream s m Char => Psc.ParsecT s u m (String,String)
codeBlock = do
  _ <- Psc.count 3 (Psc.char '`')
  (codeType, _) <- S.manyTill_ (Psc.try Psc.alphaNum) (Psc.char '\n' Psc.<|> Psc.char '\\')
  (code, _) <- S.manyTill_ (Psc.anyChar Psc.<|> Psc.char '\n') (Psc.count 3 $ Psc.char '`')
  pure (codeType, code)
  
  

-- Would be dope to have a retry strategy 

-- askGen :: Retries -> Question -> (DeepSeekResponse -> a) -> IO a




-- | Low-level Ollama chat call. Sends messages to @localhost:11434\/api\/chat@
-- (non-streaming) and returns the parsed 'DeepSeekResponse' or an error.
askDeepSeek :: MonadIO m => Manager -> DeepSeekModel -> [ContentWithRole] -> m (Either T.Text DeepSeekResponse)
askDeepSeek mgr modelDS contents = liftIO $ do
  putStrLn "askDeepSeek"
  let url = "http://localhost:11434/api/chat" 
  req <- parseRequest url
  let headers = [ (hContentType, "application/json")
                ]
  let prompt = mkDSPrompt modelDS contents -- [ cwr User contents ]
  let req' = req { requestHeaders = (fmap . fmap) (T.encodeUtf8 . T.pack) headers
                 , method = "POST"
                 , responseTimeout = responseTimeoutNone
                 , requestBody = RequestBodyLBS $ Aeson.encode prompt --txt
                 }

  response_ :: Either HttpException LBS.ByteString <- (CE.try $ fmap responseBody $ httpLbs req' mgr)
  case response_ of 
    Left (e :: HttpException) -> pure $ Left $ T.pack $ (show e)
    Right resBody -> do
      case eitherDecode resBody :: Either String DeepSeekResponse of
        Left e -> do
          pure . Left . T.pack $ e
        Right a -> pure $ Right  a 




-- | Ask GPT and decode the response as JSON into type @b@.
-- Returns @Right Nothing@ if the response is valid text but not valid JSON for @b@.
askGPTJSON
  :: FromJSON b
  => APIKey 'OpenAI
  -> Manager
  -> TokenLimit
  -> [ContentWithRole]
  -> IO (Either T.Text (Maybe b))
askGPTJSON apiKey mgr tokenLimit contents = do
  content_ <- askGPT apiKey mgr gptModel tokenLimit contents
  print content_
  pure $ flip fmap content_ (Aeson.decode . LBS.fromStrict . T.encodeUtf8)


-- ============================================================
-- Generic conversation layer (provider-agnostic via LLM.Provider)
-- ============================================================

-- | Ask any LLM backend (via 'LLM.Provider.askLLM') with conversation context.
-- Retrieves relevant history, prepends it, and stores the Q&A pair on success.
askWithContext :: MonadIO m
  => RelevantContext -> (Tag, ConvoQuestion)
  -> ConvoT m (Either LLMError (ConvoAnswer T.Text))
askWithContext relCtx (thisTag, ConvoQuestion contents) = ConvoT $ do
  histItems <- getRelevantCtx relCtx
  result <- lift $ unLLMT $ askLLM (renderHistory histItems : contents)
  case result of
    Left e -> pure $ Left e
    Right answer -> do
      let new = ConvoQuery thisTag (ConvoQuestion contents) (ConvoAnswer answer)
      modify ((:) new)
      pure $ Right $ ConvoAnswer answer

-- | Like 'askWithContext' but parses the response as JSON into type @a@.
askJSONWithContext :: (MonadIO m, FromJValue a)
  => RelevantContext -> (Tag, ConvoQuestion)
  -> ConvoT m (Either LLMError (Maybe a))
askJSONWithContext relCtx tq = do
  result <- askWithContext relCtx tq
  return $ case result of
    Left e -> Left e
    Right (ConvoAnswer txt) -> Right (parseLLMJSON txt)

-- | Default GPT model used by 'askGPT' and friends.
gptModel :: T.Text
gptModel = "gpt-4o-2024-05-13" -- "gpt-4"

