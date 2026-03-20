{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Test.Tasty
import Test.Tasty.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import LLM.Types
import LLM.Provider
import LLM.Provider.Backends
import LLM.ReadLLM

import Data.Aeson (encode, eitherDecode, toJSON, fromJSON, Result(..))
import Data.Typeable (Typeable, typeRep, Proxy(..))
import Scrappy.JSON.Record (jString, jInt, jBool)
import Scrappy.JSON.Value (FromJValue(..), JValue(..), (.:))
import Text.Parsec (string, spaces)
import qualified Data.Text as T
import System.Directory (findExecutable)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "llm-with-context"
  [ testGroup "Types" typesTests
  , testGroup "Provider" providerTests
  , testGroup "Backends" backendsTests
  , testGroup "Integration" integrationTests
  ]

-- ============================================================
-- Generators
-- ============================================================

genRole :: Gen GPTRole
genRole = Gen.element [System, User, Assistant]

genText :: Gen T.Text
genText = Gen.text (Range.linear 0 200) Gen.unicode

genNonEmptyText :: Gen T.Text
genNonEmptyText = Gen.text (Range.linear 1 200) Gen.unicode

genContentWithRole :: Gen ContentWithRole
genContentWithRole = ContentWithRole <$> genRole <*> genText

genAnthropicContent :: Gen AnthropicContent
genAnthropicContent = AnthropicContent
  <$> Gen.element ["text", "image"]
  <*> genText

genAnthropicResponse :: Gen AnthropicResponse
genAnthropicResponse = AnthropicResponse
  <$> genNonEmptyText
  <*> Gen.list (Range.linear 1 5) genAnthropicContent
  <*> genNonEmptyText

genDeepSeekModel :: Gen DeepSeekModel
genDeepSeekModel = Gen.element [DS_1_5b, DS_7b, DS_8b, DS_14b, DS_32b, DS_70b, DS_671b]

genResponseFormat :: Gen ResponseFormat
genResponseFormat = Gen.element [AsText, AsJSON]

-- ============================================================
-- Types Tests: JSON roundtrip properties
-- ============================================================

typesTests :: [TestTree]
typesTests =
  [ testProperty "GPTRole roundtrips through JSON" prop_gptRole_roundtrip
  , testProperty "ContentWithRole roundtrips through JSON" prop_cwr_roundtrip
  , testProperty "AnthropicContent roundtrips through JSON" prop_anthropicContent_roundtrip
  , testProperty "AnthropicResponse roundtrips through JSON" prop_anthropicResponse_roundtrip
  , testProperty "DeepSeekModel roundtrips through JSON" prop_deepSeekModel_roundtrip
  , testProperty "ResponseFormat roundtrips through JSON" prop_responseFormat_roundtrip
  , testProperty "cwr helper constructs correctly" prop_cwr_helper
  , testProperty "GPTRole Eq instance is correct" prop_gptRole_eq
  ]

prop_gptRole_roundtrip :: Property
prop_gptRole_roundtrip = property $ do
  role <- forAll genRole
  tripping role toJSON fromJSON

prop_cwr_roundtrip :: Property
prop_cwr_roundtrip = property $ do
  c <- forAll genContentWithRole
  let bs = encode c
  case eitherDecode bs of
    Right (c' :: ContentWithRole) -> do
      _cwr_role c' === _cwr_role c
      _cwr_content c' === _cwr_content c
    Left err -> do
      annotate err
      failure

prop_anthropicContent_roundtrip :: Property
prop_anthropicContent_roundtrip = property $ do
  ac <- forAll genAnthropicContent
  let bs = encode ac
  case eitherDecode bs of
    Right (ac' :: AnthropicContent) -> do
      _anthropicContent_type ac' === _anthropicContent_type ac
      _anthropicContent_text ac' === _anthropicContent_text ac
    Left err -> do
      annotate err
      failure

prop_anthropicResponse_roundtrip :: Property
prop_anthropicResponse_roundtrip = property $ do
  ar <- forAll genAnthropicResponse
  let bs = encode ar
  case eitherDecode bs of
    Right (ar' :: AnthropicResponse) -> do
      _anthropicResponse_id ar' === _anthropicResponse_id ar
      _anthropicResponse_model ar' === _anthropicResponse_model ar
      length (_anthropicResponse_content ar') === length (_anthropicResponse_content ar)
    Left err -> do
      annotate err
      failure

prop_deepSeekModel_roundtrip :: Property
prop_deepSeekModel_roundtrip = property $ do
  m <- forAll genDeepSeekModel
  tripping m toJSON fromJSON

prop_responseFormat_roundtrip :: Property
prop_responseFormat_roundtrip = property $ do
  rf <- forAll genResponseFormat
  tripping rf toJSON fromJSON

prop_cwr_helper :: Property
prop_cwr_helper = property $ do
  role <- forAll genRole
  txt <- forAll genText
  let c = cwr role txt
  _cwr_role c === role
  _cwr_content c === txt

prop_gptRole_eq :: Property
prop_gptRole_eq = property $ do
  assert $ System == System
  assert $ User == User
  assert $ Assistant == Assistant
  assert $ System /= User
  assert $ User /= Assistant
  assert $ System /= Assistant

-- ============================================================
-- Provider Tests: askLLM plumbing
-- ============================================================

providerTests :: [TestTree]
providerTests =
  [ testProperty "askLLM dispatches to backend" prop_askLLM_dispatches
  , testProperty "askLLMJSON parses valid JSON" prop_askLLMJSON_parses
  , testProperty "askLLMJSON returns Nothing for non-JSON" prop_askLLMJSON_nonJson
  , testProperty "askLLM propagates errors" prop_askLLM_error
  , testProperty "askLLMJSON handles code-fenced responses" prop_askLLMJSON_fenced
  ]

-- A mock backend that echoes back the concatenated message contents
echoBackend :: LLMBackend
echoBackend = LLMBackend
  { _llmBackend_name = "test/echo"
  , _llmBackend_api  = APIMock $ \msgs ->
      pure $ Right $ T.intercalate "\n" (map _cwr_content msgs)
  }

-- A mock backend that always fails
failBackend :: LLMError -> LLMBackend
failBackend err = LLMBackend
  { _llmBackend_name = "test/fail"
  , _llmBackend_api  = APIMock $ \_ -> pure $ Left err
  }

-- A mock backend that returns valid JSON
jsonBackend :: T.Text -> LLMBackend
jsonBackend jsonStr = LLMBackend
  { _llmBackend_name = "test/json"
  , _llmBackend_api  = APIMock $ \_ -> pure $ Right jsonStr
  }

-- Simple user-facing type for testing askLLMJSON
data SimpleRecord = SimpleRecord
  { _sr_name :: String
  , _sr_age  :: Int
  } deriving (Show, Eq)

instance FromJValue SimpleRecord where
  fromJValue (JObject obj) = do
    name <- obj .: "name"
    age  <- obj .: "age"
    Just $ SimpleRecord name age
  fromJValue _ = Nothing

prop_askLLM_dispatches :: Property
prop_askLLM_dispatches = property $ do
  txt <- forAll genNonEmptyText
  let env = LLMEnv echoBackend
      msgs = [cwr User txt]
  result <- evalIO $ runLLM env (askLLM msgs)
  result === Right txt

prop_askLLMJSON_parses :: Property
prop_askLLMJSON_parses = property $ do
  let env = LLMEnv (jsonBackend "{\"name\":\"Alice\",\"age\":30}")
  result <- evalIO $ runLLM env (askLLMJSON [cwr User "test"])
  case result of
    Right (Just (r :: SimpleRecord)) -> do
      _sr_name r === "Alice"
      _sr_age r === 30
    Right Nothing -> do
      annotate "Expected Just, got Nothing"
      failure
    Left err -> do
      annotate (show err)
      failure

prop_askLLMJSON_nonJson :: Property
prop_askLLMJSON_nonJson = property $ do
  let env = LLMEnv (jsonBackend "this is not json")
  result <- evalIO $ runLLM env (askLLMJSON [cwr User "test"])
  case (result :: Either LLMError (Maybe SimpleRecord)) of
    Right Nothing -> success
    _ -> do
      annotate "Expected Right Nothing for non-JSON"
      failure

prop_askLLM_error :: Property
prop_askLLM_error = property $ do
  errMsg <- forAll genNonEmptyText
  let err = LLMHttpError errMsg
      env = LLMEnv (failBackend err)
  result <- evalIO $ runLLM env (askLLM [cwr User "test"])
  case result of
    Left (LLMHttpError msg) -> msg === errMsg
    _ -> do
      annotate "Expected Left LLMHttpError"
      failure

prop_askLLMJSON_fenced :: Property
prop_askLLMJSON_fenced = withTests 1 $ property $ do
  let env = LLMEnv (jsonBackend "Sure! Here's your JSON:\n\n```json\n{\"name\":\"Bob\",\"age\":25}\n```\n")
  result <- evalIO $ runLLM env (askLLMJSON [cwr User "test"])
  case result of
    Right (Just (r :: SimpleRecord)) -> do
      _sr_name r === "Bob"
      _sr_age r === 25
    Right Nothing -> do
      annotate "Expected Just, got Nothing"
      failure
    Left err -> do
      annotate (show err)
      failure

-- ============================================================
-- Backends Tests: construction and message formatting
-- ============================================================

backendsTests :: [TestTree]
backendsTests =
  [ testProperty "mkClaudeCLI has correct name" prop_claudeCLI_name
  , testProperty "mkClaudeCLI name includes model" prop_claudeCLI_model_in_name
  , testProperty "mkOpenAI has correct name" prop_openAI_name
  , testProperty "mkDeepSeek has correct name" prop_deepSeek_name
  , testProperty "mkClaudeAPI has correct name" prop_claudeAPI_name
  ]

prop_claudeCLI_name :: Property
prop_claudeCLI_name = withTests 1 $ property $ do
  let backend = mkClaudeCLI "haiku"
  _llmBackend_name backend === "claude-cli/haiku"

prop_claudeCLI_model_in_name :: Property
prop_claudeCLI_model_in_name = property $ do
  model <- forAll $ Gen.element ["haiku", "sonnet", "opus"]
  let backend = mkClaudeCLI model
  assert $ T.isInfixOf model (_llmBackend_name backend)

prop_openAI_name :: Property
prop_openAI_name = withTests 1 $ property $ do
  let backend = mkOpenAI (APIKey "fake") undefined "gpt-4o" Nothing
  _llmBackend_name backend === "openai/gpt-4o"

prop_deepSeek_name :: Property
prop_deepSeek_name = property $ do
  model <- forAll genDeepSeekModel
  let backend = mkDeepSeek undefined model
  assert $ T.isPrefixOf "deepseek/" (_llmBackend_name backend)

prop_claudeAPI_name :: Property
prop_claudeAPI_name = withTests 1 $ property $ do
  let backend = mkClaudeAPI (APIKey "fake") undefined "claude-sonnet-4-20250514"
  _llmBackend_name backend === "claude-api/claude-sonnet-4-20250514"

-- ============================================================
-- Integration Tests: Real LLM calls -> Haskell records
-- ============================================================

-- A simple record we ask the LLM to produce as JSON
data MovieReview = MovieReview
  { _movieReview_title  :: String
  , _movieReview_rating :: Int
  , _movieReview_summary :: String
  } deriving (Show, Eq)

instance FromJValue MovieReview where
  fromJValue (JObject obj) = do
    title   <- obj .: "title"
    rating  <- obj .: "rating"
    summary <- obj .: "summary"
    Just $ MovieReview title rating summary
  fromJValue _ = Nothing

-- A record built from N separate AI calls
data CompositeAnalysis = CompositeAnalysis
  { _ca_sentiment  :: String   -- from call 1
  , _ca_keywords   :: [String] -- from call 2
  , _ca_wordCount  :: Int      -- from call 3
  } deriving (Show, Eq)

data SentimentResult = SentimentResult
  { _sentimentResult_sentiment :: String
  } deriving (Show, Eq)

instance FromJValue SentimentResult where
  fromJValue (JObject obj) = do
    s <- obj .: "sentiment"
    Just $ SentimentResult s
  fromJValue _ = Nothing

data KeywordsResult = KeywordsResult
  { _kr_keywords :: [String]
  } deriving (Show, Eq)

instance FromJValue KeywordsResult where
  fromJValue (JObject obj) = do
    ks <- obj .: "keywords"
    Just $ KeywordsResult ks
  fromJValue _ = Nothing

data WordCountResult = WordCountResult
  { _wc_count :: Int
  } deriving (Show, Eq)

instance FromJValue WordCountResult where
  fromJValue (JObject obj) = do
    c <- obj .: "count"
    Just $ WordCountResult c
  fromJValue _ = Nothing

-- | Check if claude CLI is available
hasClaude :: IO Bool
hasClaude = do
  found <- findExecutable "claude"
  pure $ case found of
    Nothing -> False
    Just _  -> True

integrationTests :: [TestTree]
integrationTests =
  [ testProperty "askLLMJSON parses a single record from LLM" prop_llm_single_record
  , testProperty "N AI calls compose into a single record" prop_llm_composite_record
  , testProperty "5-field struct via Proxy+Typeable+search with <*>" prop_applicative_search
  , testProperty "search parses Show output from noisy LLM" prop_applicative_search_noisy
  ]

claudeEnv :: LLMEnv
claudeEnv = LLMEnv (mkClaudeCLI "haiku")

prop_llm_single_record :: Property
prop_llm_single_record = withTests 1 $ property $ do
  available <- evalIO hasClaude
  if not available
    then annotate "claude CLI not found, skipping"
    else do
      let prompt = T.unlines
            [ "Write a JSON review of the movie The Matrix."
            , "The JSON must have exactly these 3 fields:"
            , "  \"title\" (string, must be \"The Matrix\")"
            , "  \"rating\" (integer, 1-10)"
            , "  \"summary\" (string, one sentence)"
            , "Example: {\"title\":\"The Matrix\",\"rating\":9,\"summary\":\"A groundbreaking sci-fi film.\"}"
            ]
      result <- evalIO $ runLLM claudeEnv (askLLMJSON [cwr User prompt])
      case (result :: Either LLMError (Maybe MovieReview)) of
        Right (Just review) -> do
          annotate $ "Got review: " ++ show review
          _movieReview_title review === "The Matrix"
          assert $ _movieReview_rating review >= 1 && _movieReview_rating review <= 10
          assert $ length (_movieReview_summary review) > 0
        Right Nothing -> do
          annotate "askLLMJSON could not extract valid JSON from LLM response"
          failure
        Left err -> do
          annotate $ "LLM call failed: " ++ show err
          failure

prop_llm_composite_record :: Property
prop_llm_composite_record = withTests 1 $ property $ do
  available <- evalIO hasClaude
  if not available
    then annotate "claude CLI not found, skipping"
    else do
      let inputText = "Haskell is a purely functional programming language with strong static typing and lazy evaluation."

      -- Call 1: sentiment
      let sentimentPrompt = T.unlines
            [ "Analyze the sentiment of this text: \"" <> inputText <> "\""
            , "Return ONLY a JSON object: {\"sentiment\": \"positive\"} or {\"sentiment\": \"negative\"} or {\"sentiment\": \"neutral\"}"
            , "Return ONLY valid JSON, no markdown, no explanation, no code fences."
            ]
      sentimentResult <- evalIO $ runLLM claudeEnv (askLLMJSON [cwr User sentimentPrompt])
      sentiment <- case (sentimentResult :: Either LLMError (Maybe SentimentResult)) of
        Right (Just sr) -> do
          annotate $ "Sentiment: " ++ show sr
          assert $ _sentimentResult_sentiment sr `elem` ["positive", "negative", "neutral"]
          pure $ _sentimentResult_sentiment sr
        Right Nothing -> do
          annotate "Failed to parse sentiment JSON"
          failure
          pure ""
        Left err -> do
          annotate $ "Sentiment call failed: " ++ show err
          failure
          pure ""

      -- Call 2: keywords
      let keywordsPrompt = T.unlines
            [ "Extract 3 keywords from this text: \"" <> inputText <> "\""
            , "Return ONLY a JSON object: {\"keywords\": [\"word1\", \"word2\", \"word3\"]}"
            , "Return ONLY valid JSON, no markdown, no explanation, no code fences."
            ]
      keywordsResult <- evalIO $ runLLM claudeEnv (askLLMJSON [cwr User keywordsPrompt])
      keywords <- case (keywordsResult :: Either LLMError (Maybe KeywordsResult)) of
        Right (Just kr) -> do
          annotate $ "Keywords: " ++ show kr
          assert $ length (_kr_keywords kr) > 0
          pure $ _kr_keywords kr
        Right Nothing -> do
          annotate "Failed to parse keywords JSON"
          failure
          pure []
        Left err -> do
          annotate $ "Keywords call failed: " ++ show err
          failure
          pure []

      -- Call 3: word count
      let wcPrompt = T.unlines
            [ "Count the number of words in this text: \"" <> inputText <> "\""
            , "Return ONLY a JSON object: {\"count\": N} where N is the integer word count."
            , "Return ONLY valid JSON, no markdown, no explanation, no code fences."
            ]
      wcResult <- evalIO $ runLLM claudeEnv (askLLMJSON [cwr User wcPrompt])
      wc <- case (wcResult :: Either LLMError (Maybe WordCountResult)) of
        Right (Just w) -> do
          annotate $ "Word count: " ++ show w
          assert $ _wc_count w > 0
          pure $ _wc_count w
        Right Nothing -> do
          annotate "Failed to parse word count JSON"
          failure
          pure 0
        Left err -> do
          annotate $ "Word count call failed: " ++ show err
          failure
          pure 0

      -- Compose into final record
      let composite = CompositeAnalysis
            { _ca_sentiment = sentiment
            , _ca_keywords  = keywords
            , _ca_wordCount = wc
            }
      annotate $ "Composite record: " ++ show composite
      assert $ _ca_sentiment composite `elem` ["positive", "negative", "neutral"]
      assert $ length (_ca_keywords composite) > 0
      assert $ _ca_wordCount composite > 0

-- ============================================================
-- Applicative search test: 5-field struct via Proxy + Typeable + <*>
-- ============================================================

data BookAnalysis = BookAnalysis
  { _ba_title       :: String
  , _ba_author      :: String
  , _ba_yearPublished :: Int
  , _ba_pageCount   :: Int
  , _ba_isClassic   :: Bool
  } deriving (Show, Eq)

-- | Build a prompt that tells the LLM to return a specific Haskell type,
-- using Typeable to get the type name (same pattern as gptReturnType).
llmReturnType :: forall a. Typeable a => Proxy a -> T.Text
llmReturnType proxy =
  let typeInfo = T.pack $ show (typeRep proxy)
  in "Respond with ONLY a value of Haskell type: " <> typeInfo <> ". No explanation, no markdown."

-- | Ask the LLM for a typed value, using Proxy+Typeable to build the prompt
-- and ReadLLM's search to parse the response.
askFieldSearch
  :: forall a. (ReadLLM a, Typeable a)
  => T.Text  -- ^ The question
  -> LLMEnv
  -> IO (Either LLMError (Maybe a))
askFieldSearch question env = do
  let typeHint = llmReturnType (Proxy :: Proxy a)
      msgs = [cwr User question, cwr System typeHint]
  result <- runLLM env (askLLM msgs)
  pure $ case result of
    Left e -> Left e
    Right txt ->
      let found = search (T.unpack txt)
      in Right $ case found of
        Just (x:_) -> Just x
        _          -> Nothing

-- | Mock backend that simulates per-field LLM responses for BookAnalysis
bookFieldBackend :: LLMBackend
bookFieldBackend = LLMBackend
  { _llmBackend_name = "test/book-fields"
  , _llmBackend_api  = APIMock $ \msgs ->
      let content = T.intercalate " " (map _cwr_content msgs)
      in pure . Right $ if T.isInfixOf "title" content
        then "\"To Kill a Mockingbird\""
        else if T.isInfixOf "author" content
        then "\"Harper Lee\""
        else if T.isInfixOf "year" content
        then "1960"
        else if T.isInfixOf "page" content
        then "281"
        else if T.isInfixOf "classic" content
        then "true"
        else "unknown"
  }

prop_applicative_search :: Property
prop_applicative_search = withTests 1 $ property $ do
  let env = LLMEnv bookFieldBackend

  -- 5 separate calls, each using Proxy+Typeable for prompt + search for parsing
  r1 <- evalIO $ askFieldSearch @String "What is the title of the book?" env
  r2 <- evalIO $ askFieldSearch @String "Who is the author of the book?" env
  r3 <- evalIO $ askFieldSearch @Int "What year was the book published?" env
  r4 <- evalIO $ askFieldSearch @Int "How many pages does the book have?" env
  r5 <- evalIO $ askFieldSearch @Bool "Is this book considered a classic?" env

  -- Applicative composition: extract each Right (Just x) and combine with <*>
  let result = BookAnalysis
        <$> extract r1
        <*> extract r2
        <*> extract r3
        <*> extract r4
        <*> extract r5

  case result of
    Just book -> do
      annotate $ "Got book: " ++ show book
      _ba_title book === "To Kill a Mockingbird"
      _ba_author book === "Harper Lee"
      _ba_yearPublished book === 1960
      _ba_pageCount book === 281
      _ba_isClassic book === True
    Nothing -> do
      annotate $ "Failed to compose. r1=" ++ show r1 ++ " r2=" ++ show r2
        ++ " r3=" ++ show r3 ++ " r4=" ++ show r4 ++ " r5=" ++ show r5
      failure
  where
    extract :: Either LLMError (Maybe a) -> Maybe a
    extract (Right (Just a)) = Just a
    extract _ = Nothing

-- ============================================================
-- Noisy LLM test: search parses Show output from chatty responses
-- ============================================================

newtype BookTitle = BookTitle String deriving (Show, Eq)
newtype AuthorName = AuthorName String deriving (Show, Eq)
newtype YearPublished = YearPublished Int deriving (Show, Eq)
newtype PageCount = PageCount Int deriving (Show, Eq)
newtype IsClassic = IsClassic Bool deriving (Show, Eq)

instance ReadLLM BookTitle where
  readLLM = do
    _ <- string "BookTitle"
    spaces
    BookTitle <$> jString

instance ReadLLM AuthorName where
  readLLM = do
    _ <- string "AuthorName"
    spaces
    AuthorName <$> jString

instance ReadLLM YearPublished where
  readLLM = do
    _ <- string "YearPublished"
    spaces
    YearPublished <$> jInt

instance ReadLLM PageCount where
  readLLM = do
    _ <- string "PageCount"
    spaces
    PageCount <$> jInt

instance ReadLLM IsClassic where
  readLLM = do
    _ <- string "IsClassic"
    spaces
    IsClassic <$> jBool

data BookAnalysis2 = BookAnalysis2
  { _ba2_title       :: BookTitle
  , _ba2_author      :: AuthorName
  , _ba2_yearPublished :: YearPublished
  , _ba2_pageCount   :: PageCount
  , _ba2_isClassic   :: IsClassic
  } deriving (Show, Eq)

-- | Mock backend that wraps Show-formatted values in chatty prose
noisyBookBackend :: LLMBackend
noisyBookBackend = LLMBackend
  { _llmBackend_name = "test/noisy-book"
  , _llmBackend_api  = APIMock $ \msgs ->
      let content = T.intercalate " " (map _cwr_content msgs)
      in pure . Right $ if T.isInfixOf "title" content
        then "Sure! The title of the book is BookTitle \"To Kill a Mockingbird\" which is a classic of American literature."
        else if T.isInfixOf "author" content
        then "Great question! The author is AuthorName \"Harper Lee\" who won the Pulitzer Prize in 1961."
        else if T.isInfixOf "year" content
        then "The book was originally published as YearPublished 1960 during the Civil Rights era in the United States."
        else if T.isInfixOf "page" content
        then "In most standard editions, the novel is PageCount 281 pages long, though this can vary."
        else if T.isInfixOf "classic" content
        then "Absolutely, this is considered IsClassic true by any measure of literary merit and cultural impact."
        else "I'm not sure what you're asking about."
  }

prop_applicative_search_noisy :: Property
prop_applicative_search_noisy = withTests 1 $ property $ do
  let env = LLMEnv noisyBookBackend

  r1 <- evalIO $ askFieldSearch @BookTitle "What is the title of the book?" env
  r2 <- evalIO $ askFieldSearch @AuthorName "Who is the author of the book?" env
  r3 <- evalIO $ askFieldSearch @YearPublished "What year was the book published?" env
  r4 <- evalIO $ askFieldSearch @PageCount "How many pages does the book have?" env
  r5 <- evalIO $ askFieldSearch @IsClassic "Is this book considered a classic?" env

  let result = BookAnalysis2
        <$> extract r1
        <*> extract r2
        <*> extract r3
        <*> extract r4
        <*> extract r5

  case result of
    Just book -> do
      annotate $ "Got book: " ++ show book
      _ba2_title book === BookTitle "To Kill a Mockingbird"
      _ba2_author book === AuthorName "Harper Lee"
      _ba2_yearPublished book === YearPublished 1960
      _ba2_pageCount book === PageCount 281
      _ba2_isClassic book === IsClassic True
    Nothing -> do
      annotate $ "Failed to compose. r1=" ++ show r1 ++ " r2=" ++ show r2
        ++ " r3=" ++ show r3 ++ " r4=" ++ show r4 ++ " r5=" ++ show r5
      failure
  where
    extract :: Either LLMError (Maybe a) -> Maybe a
    extract (Right (Just a)) = Just a
    extract _ = Nothing
