{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module LLM where

import Types

import Scrappy.Elem as S hiding (Tag)
import Scrappy.Scrape as S

import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header

import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Control.Monad (forM)
import Control.Exception as CE
import Data.Bifunctor
import Data.Aeson as Aeson
import Text.Parsec as Psc
import Data.Typeable
import Data.Default
import Text.Read (readEither)
import Data.Maybe (catMaybes)
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as LBS
import System.Directory (doesFileExist)

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
getScriptSectionText :: DeepSeekAnswer -> Either Psc.ParseError T.Text
getScriptSectionText = fmap (mconcat . _thoughtResponse_answer ) . toThoughtResponse . \(GPTAnswer x) -> x

renderSection :: ScriptSectionPlan -> T.Text
renderSection sect = mconcat [ _sectionPlan_title sect
                             , " ensure a word count of "
                             , tshow $ _sectionPlan_wordCount sect
                             , "!"
                             , " based on the summary/overview: "
                             , _sectionPlan_outline sect
                             ] 


data GetSectionError = NoCodeBlock | WrongCodeBlock | NoThoughtResponse | FailedAesonParse
getSections :: DeepSeekAnswer -> Either String [ScriptSectionPlan] 
getSections (GPTAnswer cwr_) =
  let
    thoughtRes = toThoughtResponse cwr_
    -- getAnswerAsString = T.unpack . mconcat . _thoughtResponse_answer
  in
    case thoughtRes of
      Left parseError -> Left $ "Couldn't create ThoughtResponse: " <> show parseError
      Right _thRes ->
        let mCodeBlock = S.scrapeFirst' codeBlock $ T.unpack $ _cwr_content cwr_ --  $ getAnswerAsString thRes
        in case mCodeBlock of
          Nothing -> Left "No code block found"
          Just (codeType, cblock) -> 
            case codeType of
              "json" -> 
                case (Aeson.eitherDecode (LBS.fromStrict . T.encodeUtf8 . T.pack $ cblock)) of
                  -- the most likely case for error here is a comment in the JSON codeblock
                  Left e -> Left $  "Failed at JSON parser step: " <> show e
                  Right (plan :: [ScriptSectionPlan]) -> Right plan 
              _ -> Left $ "received different type of codeblock: " <> codeType



channelGenericInstructions :: T.Text
channelGenericInstructions =
  "the voiceover / script will be continuous throughout the video so there will be no pauses for\
  \ clips. Please factor this in. Also my audience loves tidbits of football analysis and facts.\
  \ Please use these to back up any claims about a play or game or player’s ability. For example\
  \ if I say it was a great game. It would be better to say this was the first game all season\
  \ where the number 1 and number 2 ranked teams were facing off. Another example is that Tebow\
  \ is a left handed QB which is rare so if it had been 10 years since a left handed QB had won\
  \ a playoff game (or achieved any other significant stat) it is probably worth mentioning in\
  \ order to bolster the claim and gain the audiences trust. Trust with the audience is by far\
  \ the most important thing because our viewers are highly knowledgeable and love learning\
  \ things about football that they never knew "
  <> "for more in-depth information on the audience, the following is a psychographic analysis on the audience"
  <> "\n" <> psychographic <> "\n"

psychographic = ""
-- psychographic :: T.Text
-- psychographic = T.pack $ [istr|
-- FOOTBALL NICHE VIDEO IDEAS

-- **Psychographic Questions**
-- - What sort of language do they connect with
--     - simple
--     - common sporting terms peak their respect for the speaker
--     - analytical and comparative when talking sports
-- - What stage of their journey are they at? (beginner vs expert)
--     - quite a range but likely to be at least "almost medium" level
--     - More likely to be expert than a beginner if they are watching  a YT video
-- - Are they here to learn or to be entertained
--     - to be entertained
--     - small bit of learning (to apply to Fantasy) but not here for it
-- - What TV/media do they watch?
--     - All kinds of sports 
--     - Sundays are for football
--     - likely a 2nd sport (in order of probability)
--         - MMA 
--         - Baseball
--         - Hockey
--         - Basketball
--         - Rugby
--         - Soccer
--     - Probably Right-leaning media
-- - What are their hobbies
--     - Beer leagues
--     - Video Games
-- - Whats their ultimate goal
--     - be entertained and be a football guru
-- |]

runStateAction :: MonadIO m => Manager -> DeepSeekModel -> [ContentWithRole] -> MonadDeepSeek m (Either GPTError DeepSeekAnswer)
runStateAction mgr modelChoice cwrs = askDeepSeekWithContext mgr modelChoice (LastN_DS 1000) (TagDS "sometag" False, DeepSeekQuestion cwrs)

data OutlineParams
  = TotalTime Seconds
  | TimedSection [(SectionName, SectionLength)]
  | MandatorySections [SectionName]
  
type SectionName = T.Text
type Seconds = Int 
type SectionLength = Seconds

data ScriptCategory
  = MEAN
  | RiseAndFall
  | Underdog
  | Emotional
  | HowBad
  | HowGood
  | StatisticalClub
  | IsBestOfAllTime
  | IsWorstOfAllTime
  deriving (Eq,Ord)

categoryMap :: Map.Map ScriptCategory T.Text
categoryMap = Map.fromList
  [ (MEAN, "")
  , (RiseAndFall,"")
  , (Underdog, "")
  , (Emotional, "")
  , (HowBad,"")
  , (HowGood,"This video will focus on how good a player was. Meaning the script should provide a chronological overview of times they have done horribly and times they have been great.")
  , (StatisticalClub, "")
  , (IsBestOfAllTime,"")
  , (IsWorstOfAllTime,"")
  ]



-- | An example case would be "who are the 10 biggest busts in NFL history? rank by params: {...}"
createScriptPlan :: T.Text -> IO ScriptPlan
createScriptPlan _title = undefined

data ScriptPlan = ScriptPlan
  { _script_title :: T.Text
  , _script_category :: ScriptCategory
  , _script_description :: T.Text
  , _script_outline :: OutlineParams
  , _script_keyFacts :: [T.Text]  
  , _script_style :: T.Text -- which emotions, whats the balance of storytelling vs stat sharing?
  , _script_remindSection :: T.Text -- important prompt-info that the model should be reminded about during creation of each section
  }

myScript :: ScriptPlan 
myScript = ScriptPlan
  { _script_title = "How GOOD was Tim Tebow?"
  , _script_category = HowGood
  , _script_description = "how Tim Tebow had a legendary college career as well as a controversial NFL career with lots of highs and lows."
  , _script_outline = TotalTime (12 * 60)
  , _script_keyFacts = []
  , _script_style = "Viewers should feel inspired"
  , _script_remindSection = ""
  }

writeFromScriptPlan :: DeepSeekModel -> ScriptPlan -> IO ()
writeFromScriptPlan modelChoice plan = do 
  writeScript
    modelChoice
    (_script_title plan)
    catDesc
    (_script_description plan)
    outlineInst
    keyFacts
    (_script_style plan)
    (_script_remindSection plan)
  where
    catDesc = categoryMap Map.! (_script_category plan)
    outlineInst = renderOutlineInstructions (_script_outline plan) 
    keyFacts = renderKeyFacts (_script_keyFacts plan)

renderKeyFacts :: [T.Text] -> T.Text
renderKeyFacts = T.intercalate "\n" . fmap ("-" <>) 
    
renderOutlineInstructions :: OutlineParams -> T.Text
renderOutlineInstructions = \case
  TotalTime s -> "ensure that the total time of the video is " <> (tshow s) <> " seconds."
  TimedSection sects -> "ensure that the following sections are included: " <> (T.intercalate "," $ renderSect <$> sects) <> "."
  MandatorySections sects -> "ensure that you include the following sections " <> (T.intercalate ", " sects) <> "."  
  where
    renderSect (n,l) = n <> " for roughly " <> (tshow l) <> " seconds"   
    
writeScript :: DeepSeekModel -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> IO () 
writeScript modelChoice title categoryInstructions vidDesc outlineInstructions keyFacts scriptStyle sectionReminders = do
  mgr <- newManager tlsManagerSettings
  --script <- readFile "damar.txt" 
  (eithScript :: Either () [ScriptSectionComplete], _finalState) <- flip runStateT [] $ do
    dsAnswer <- runStateAction mgr modelChoice 
      [ cwr User $ mconcat
        [ "can you please write me a youtube voiceover script for the title: "
        , title
        , "the video description is "
        , vidDesc <> scriptStyle         
        , "for this kind of video please note: "
        , categoryInstructions
        , " also "
        , channelGenericInstructions
        , "\n"
        , ( if keyFacts /= ""
            then "for this video and content choices you make, keep in mind the following key facts: " <> keyFacts
            else ""
          ) 
        , "this will be a multi-step process, so let's start with an overview of the script."
        , " for the overview you craft please ", outlineInstructions
        , "The speaker of this script will average 215 words per minute."
        , " The first step will be creating an overview of what the script should be. In your response, as an array of the following \
          \JSON structure. For each element of the JSON array representing a section, follow this structure exactly: "
        , T.decodeUtf8 $ LBS.toStrict $ Aeson.encode (def :: ScriptSectionPlan)
        ]
      ]
    liftIO $ putStrLn "got dsAnswer"
    case dsAnswer of
      Left e -> do
        liftIO $ print e
        liftIO $ putStrLn "Failed get overview"
        pure $ Left ()
      Right dsAnswer' -> do 
        -- use code block scraper + decode block with Aeson.decode
        let sections' = getSections dsAnswer'
        case sections' of
          Left e -> do
            liftIO $ putStrLn e
            liftIO $ Aeson.encodeFile "getSectionsFailure.json" dsAnswer'
            pure $ Left ()
          Right sections -> do
            let numTODO = length sections 
            x :: [Either () ScriptSectionComplete] <- forM (zip [1 :: Int ..] sections) $ \(sectNum, section) -> do
              liftIO $ do
                putStrLn $ "Working on section " <> (show sectNum) <> "/" <> (show numTODO)
                putStrLn $ "SectionTitle: (" <> (show $ _sectionPlan_wordCount section) <> "): " <> (T.unpack $ _sectionPlan_title section) 
              dsSectionAnswer <- runStateAction mgr modelChoice
                [ cwr User $ mconcat
                  [ "Now Write: "
                  , renderSection section
                  , sectionReminders
                  ]
                ]
              case dsSectionAnswer of
                Left _ -> do
                  liftIO $ print $ "Failed get section for " <> show section
                  pure $ Left ()
                Right dsSectionAnswer' -> do
                  case getScriptSectionText dsSectionAnswer' of
                    Left e -> do
                      liftIO $ putStrLn $ "Failed get section text via thoughtResponse answer" <> show e
                      pure $ Left ()
                    Right scriptSectionText -> do 
                      pure $ Right $ ScriptSectionComplete
                        (_sectionPlan_title section)
                        (_sectionPlan_numberOfSeconds section)
                        (_sectionPlan_wordCount section)
                        scriptSectionText 
            pure $ sequenceA x

  Aeson.encodeFile "history.json" (_finalState)
  case eithScript of
    Left _ -> putStrLn "error happened" 
    Right (script :: [ScriptSectionComplete]) -> do
      Aeson.encodeFile "script.json" script
      writeNextAvailableFile (T.unpack title) ".txt" $ renderScript script 
  pure ()

renderScript :: [ScriptSectionComplete] -> T.Text
renderScript = T.intercalate "\n" . fmap renderScriptSectionComplete  

renderScriptSectionComplete :: ScriptSectionComplete -> T.Text
renderScriptSectionComplete s = T.intercalate "\n"
  [ "###" <> (_section_title s)
  , "####" <> (tshow $ _section_wordCount s) <> "words |" <> (tshow $ _section_numberOfSeconds s) <> " seconds"
  , _section_text s 
  ]
  

findNextAvailableFile :: FilePath -> String -> IO FilePath
findNextAvailableFile base ext = go 1
  where
    go :: Int -> IO FilePath
    go n = do
      let candidate = base ++ "-" ++ show n ++ ext
      doesFileExist candidate >>= \case
        True -> go (n+1)
        False -> return candidate
      -- exists_ <- doesFileExist candidate
      -- if exists_
      --   then go (n + 1)
      --   else return candidate

writeNextAvailableFile :: FilePath -> String -> T.Text -> IO ()
writeNextAvailableFile base ext str = do
  fp <- findNextAvailableFile base ext
  T.writeFile fp str

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

