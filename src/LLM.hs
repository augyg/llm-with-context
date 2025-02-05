{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLM where

import Types

import Scrappy.Elem as S hiding (Tag)

import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Types.Header

import Control.Monad.IO.Class
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

tshow :: Show a => a -> T.Text
tshow = T.pack . show 



mkDSPrompt :: DeepSeekModel -> [ContentWithRole] -> DeepSeekRequestBody
mkDSPrompt dsModel cwrs = def
  { _deepSeekRequest_model = dsModel
  , _deepSeekRequest_messages = cwrs
  }
  


getRelevant :: RelevantContext
getRelevant = LastNRelevant 10 $ \(Tag t) -> T.isPrefixOf "html" t

renderHistory :: ConversationHistory -> ContentWithRole
renderHistory = cwr Assistant . ((<>) "Our conversation history so far:") . T.intercalate "\n" . fmap renderItem
  where
    renderItem (GPTQuery _ (GPTQuestion q) (GPTAnswer a)) =
      "Me: " <> (T.decodeUtf8 . LBS.toStrict . Aeson.encode) q <> "\n" <> "ChatGPT: " <> a




getRelevantCtx :: MonadIO m => RelevantContext -> MonadGPT m ConversationHistory
getRelevantCtx = \case
  LastN n -> gets (take n)
  Relevants tags -> gets (flip finds tags)
  LastNRelevant n anonF -> gets (\x ->
                               take n
                               . filter (anonF . _gptQuery_tag) $ x
                            )
  where
    finds hist tags =
      catMaybes $ fmap (\t -> L.find (\h -> t == _gptQuery_tag h) hist) tags

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
  -> (Tag, GPTQuestion)
  -> MonadGPT m (Either GPTError (GPTAnswer a))
askGPTWithContextTyped key mgr tokenLimit relCtx (thisTag, GPTQuestion contents) = do
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
  askGPT key mgr tokenLimit (ctx : contents <> returnT) >>= \case
    Left e -> pure . Left . GPTError $ e
    Right txt -> case readEitherText txt of
      Left e -> pure . Left . GPTError $
        e <> "When reading return type: (x :: "  <> (T.pack . show $ typeRep proxy ) <> ") from base response: " <> txt
        <> "From Prompt: "
        <> (T.pack $ show (ctx : contents <> returnT))

      Right typed -> do
        let new = GPTQuery thisTag (GPTQuestion contents) (GPTAnswer txt)
        modify ((:) new)
        pure . Right . GPTAnswer $ typed

askGPTWithContext
  :: MonadIO m
  => APIKey 'OpenAI
  -> Manager
  -> TokenLimit
  -> RelevantContext
  -> (Tag, GPTQuestion)
  -> MonadGPT m (Either GPTError (GPTAnswer T.Text))
askGPTWithContext key mgr maxTokens relCtx (thisTag, GPTQuestion contents) = do
  histItems <- getRelevantCtx relCtx
  askGPT key mgr maxTokens (renderHistory histItems : contents) >>= \case
    Left e -> pure $ Left $ GPTError e
    Right answer -> do
      let new = GPTQuery thisTag (GPTQuestion contents) (GPTAnswer answer)
      modify ((:) new)
      pure $ Right $ GPTAnswer answer





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


askDeepSeekWithContext
  :: MonadIO m
  => Manager
  -> DeepSeekModel
  -> RelevantContextDS
  -> (TagDS, DeepSeekQuestion)
  -> MonadDeepSeek m (Either GPTError DeepSeekAnswer)
askDeepSeekWithContext mgr modelDS relCtx (thisTag, DeepSeekQuestion contents) = do

  
  
  histItems <- getRelevantCtxDeepSeek relCtx
  let
    historyAtNow = (mconcat $ reverse $ fmap snd histItems)
    fullCWRs = historyAtNow <> contents -- [1,2] <> [3] --> [1,2,3]

  deepSeekResult <- askDeepSeek mgr modelDS $ fullCWRs

  case deepSeekResult of
    Left e -> pure $ Left . GPTError $ e
    Right res -> do
      let newAnswer = _deepSeekResponse_message res
      modify (\state_ ->
                let question = (thisTag, contents)
                    answer = (TagDS (unTagDS thisTag) True, [newAnswer])
                in
                  answer : question : state_ 
             )  
      pure $ Right . GPTAnswer $ newAnswer

-- | TODO: Configure temperature for less variability
-- | Todo: we should probably use scrappy here so that we dont care about prefixing/position
askGPTTyped
  :: forall m a.
  ( MonadIO m
  , Typeable a
  , Read a
  )
  => APIKey 'OpenAI
  -> Manager
  -> TokenLimit
  -> GPTQuestion
  -> m (Either GPTError (GPTAnswer a))
askGPTTyped apiKey mgr maxTokens (GPTQuestion prompt) = do
  let typeProxy = Proxy :: Proxy a
  let returnT = gptReturnType typeProxy -- "Please only respond with nothing but the haskell type Map Int Int"
  let
    readEitherText :: T.Text -> Either T.Text a
    readEitherText = first T.pack . readEither2 . T.unpack
      where readEither2 x = case readEither x of
              Right a -> Right a
              Left _ -> readEither $ "\"" <> escape x <> "\""
                --Right a -> Right a


  r <- askGPT apiKey mgr maxTokens $ prompt <> returnT
  pure . bimap GPTError GPTAnswer $ readEitherText =<< r

escapeText :: T.Text -> T.Text
escapeText = T.concatMap escapeChar
  where
    escapeChar '\"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar c    = T.singleton c


escape :: String -> String
escape = concatMap esc
    where
        escchars :: String
        escchars = "$\\^.*~[]"
        esc c   | c `elem` escchars = ['\\',c]
                | otherwise         = [c]

gptReturnType :: forall a. Typeable a => Proxy a -> [ContentWithRole]
gptReturnType typeProxy =
  let
    typeInfo = T.pack $ show (typeRep typeProxy)
  in
    if typeInfo == "Text" || typeInfo == "String"
    then []
    else [ cwr System $ "In responding to the above question, give me only the Haskell type:" <> typeInfo <> " and nothing else in your response: Format should be parsable as the Haskell type:" <> typeInfo ]

type TokenLimit = Maybe Int
-- | TODO: change to gptPrim
askGPT :: MonadIO m => APIKey 'OpenAI -> Manager -> TokenLimit -> [ContentWithRole] -> m (Either T.Text T.Text)
askGPT apiKey mgr maxTokens contents = liftIO $ do
  putStrLn "askGPT"
  let url = "https://api.openai.com/v1/chat/completions"
  req <- parseRequest url
  let headers = [ (hAuthorization, "Bearer " <> (T.unpack . T.strip . unAPIKey $ apiKey))
                , (hContentType, "application/json")
                ]
  let promptLen = maybe [] (\_len -> [cwr System $ "Please limit response to " <> (T.pack $ show (50 :: Integer)) <> " tokens"]) maxTokens
  -- We add 50 to limit because as the request gets larger GPT is worse at knowing when to stop
  -- This should not affect shorter responses
  let prompt = GPTRequestBody gptModel ((+ 50) <$> maxTokens) $ promptLen <> contents
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
        chcs -> pure . Right . _cwr_content . message . head $ chcs


-- curl -X POST http://localhost:11434/api/generate      -d '{
--            "model": "deepseek-r1:1.5b",
--            "prompt": "Write a haiku about the moon.",
--            "stream": true
--          }'      -H "Content-Type: application/json"



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

codeBlock :: Psc.Stream s m Char => Psc.ParsecT s u m (String,String)
codeBlock = do
  _ <- Psc.count 3 (Psc.char '`')
  (codeType, _) <- S.manyTill_ (Psc.try Psc.alphaNum) (Psc.char '\n' Psc.<|> Psc.char '\\')
  (code, _) <- S.manyTill_ (Psc.anyChar Psc.<|> Psc.char '\n') (Psc.count 3 $ Psc.char '`')
  pure (codeType, code)
  
  

-- Would be dope to have a retry strategy 

-- askGen :: Retries -> Question -> (DeepSeekResponse -> a) -> IO a




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
        Left e -> pure . Left . T.pack $ e
        Right a -> pure $ Right  a 




askGPTJSON
  :: FromJSON b
  => APIKey 'OpenAI
  -> Manager
  -> TokenLimit
  -> [ContentWithRole]
  -> IO (Either T.Text (Maybe b))
askGPTJSON apiKey mgr tokenLimit contents = do
  content_ <- askGPT apiKey mgr tokenLimit contents
  print content_
  pure $ flip fmap content_ (Aeson.decode . LBS.fromStrict . T.encodeUtf8)


gptModel :: T.Text
gptModel = "gpt-4o-2024-05-13" -- "gpt-4"

