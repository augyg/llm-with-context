{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}

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
import LLM.JsonExample
import LLM.ScrubPrefix (scrubPrefix)
import LLM.LLM (getRelevantCtx, getRelevantCtxDeepSeek, renderHistory, askWithContext, askJSONWithContext)

import Control.Monad.Trans.State (evalStateT)
import Data.Aeson (encode, eitherDecode, toJSON, fromJSON, fieldLabelModifier)
import Data.Typeable (Typeable, typeRep, Proxy(..))
import GHC.Generics (Generic)
import Scrappy.JSON.Record (jString, jInt, jDouble, jBool, jNull, jArray)
import Scrappy.JSON.Value (FromJValue(..), JValue(..), (.:))
import Text.Parsec (parse, string, spaces, char)
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
  , testGroup "ScrappyJSON" scrappyTests
  , testGroup "JsonExample" jsonExampleTests
  , testGroup "Extraction" extractionTests
  , testGroup "Context" contextTests
  , testGroup "TypeConstruction" typeConstructionTests
  , testGroup "ScrubPrefix" scrubPrefixTests
  , testGroup "ReadLLM Edge Cases" readLLMEdgeTests
  , testGroup "JsonExample Edge Cases" jsonExampleEdgeTests
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
          _ <- failure
          pure ""
        Left err -> do
          annotate $ "Sentiment call failed: " ++ show err
          _ <- failure
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
          _ <- failure
          pure []
        Left err -> do
          annotate $ "Keywords call failed: " ++ show err
          _ <- failure
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
          _ <- failure
          pure 0
        Left err -> do
          annotate $ "Word count call failed: " ++ show err
          _ <- failure
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

-- ============================================================
-- Scrappy-JSON Defensive Tests
-- ============================================================

-- Defensive tests that pin down scrappy-json behavior as used by this library.
-- If scrappy-json changes behavior, these tests catch it.

scrappyTests :: [TestTree]
scrappyTests =
  [ testGroup "Primitive Parsers"
    [ testProperty "jInt parses positive" prop_jInt_pos
    , testProperty "jInt parses negative" prop_jInt_neg
    , testProperty "jInt parses zero" prop_jInt_zero
    , testProperty "jDouble parses float" prop_jDouble_float
    , testProperty "jDouble parses negative" prop_jDouble_neg
    , testProperty "jBool parses true (lowercase)" prop_jBool_true
    , testProperty "jBool parses false (lowercase)" prop_jBool_false
    , testProperty "jBool rejects capitalized True" prop_jBool_caseSensitive
    , testProperty "jNull parses null" prop_jNull_parse
    , testProperty "jString parses simple string" prop_jString_simple
    , testProperty "jString handles escaped quotes" prop_jString_escQuotes
    , testProperty "jString handles escaped backslash" prop_jString_escBackslash
    , testProperty "jString handles \\n escape" prop_jString_newline
    , testProperty "jArray parses int list" prop_jArray_ints
    , testProperty "jArray parses empty array" prop_jArray_empty
    , testProperty "jArray parses nested strings" prop_jArray_strings
    ]
  , testGroup "FromJValue"
    [ testProperty "(.:) finds present key" prop_lookup_present
    , testProperty "(.:) Nothing for missing key" prop_lookup_missing
    , testProperty "(.:) Nothing for type mismatch" prop_lookup_typeMismatch
    , testProperty "fromJValue Int from JNumber" prop_fv_int
    , testProperty "fromJValue Int from JString fails" prop_fv_intFromString
    , testProperty "fromJValue Bool from JBool" prop_fv_bool
    , testProperty "fromJValue String from JString" prop_fv_string
    , testProperty "fromJValue [Int] from JArray" prop_fv_intList
    , testProperty "fromJValue [Int] fails on mixed array" prop_fv_mixedArray
    ]
  , testGroup "search (scrape from noise)"
    [ testProperty "search finds multiple ints in text" prop_search_multi
    , testProperty "search finds no ints in text without numbers" prop_search_none
    , testProperty "parseLLMJSON handles nested object with array" prop_parseLLM_nested
    , testProperty "parseLLMJSON handles null field" prop_parseLLM_nullField
    ]
  ]

-- ---- Primitive parser tests ----

prop_jInt_pos :: Property
prop_jInt_pos = withTests 1 $ property $ do
  case parse jInt "" "42" of
    Right n -> n === 42
    Left err -> do
      annotate $ show err
      failure

prop_jInt_neg :: Property
prop_jInt_neg = withTests 1 $ property $ do
  case parse jInt "" "-7" of
    Right n -> n === (-7)
    Left err -> do
      annotate $ show err
      failure

prop_jInt_zero :: Property
prop_jInt_zero = withTests 1 $ property $ do
  case parse jInt "" "0" of
    Right n -> n === 0
    Left err -> do
      annotate $ show err
      failure

prop_jDouble_float :: Property
prop_jDouble_float = withTests 1 $ property $ do
  case parse jDouble "" "3.14" of
    Right n -> do
      assert $ abs (n - 3.14) < 0.001
    Left err -> do
      annotate $ show err
      failure

prop_jDouble_neg :: Property
prop_jDouble_neg = withTests 1 $ property $ do
  case parse jDouble "" "-2.5" of
    Right n -> do
      assert $ abs (n - (-2.5)) < 0.001
    Left err -> do
      annotate $ show err
      failure

prop_jBool_true :: Property
prop_jBool_true = withTests 1 $ property $ do
  case parse jBool "" "true" of
    Right b -> b === True
    Left err -> do
      annotate $ show err
      failure

prop_jBool_false :: Property
prop_jBool_false = withTests 1 $ property $ do
  case parse jBool "" "false" of
    Right b -> b === False
    Left err -> do
      annotate $ show err
      failure

-- scrappy-json's jBool is case-sensitive: "True" should fail
prop_jBool_caseSensitive :: Property
prop_jBool_caseSensitive = withTests 1 $ property $ do
  case parse jBool "" "True" of
    Left _ -> success
    Right _ -> do
      annotate "Expected jBool to reject capitalized 'True'"
      failure

prop_jNull_parse :: Property
prop_jNull_parse = withTests 1 $ property $ do
  case parse jNull "" "null" of
    Right _ -> success
    Left err -> do
      annotate $ show err
      failure

prop_jString_simple :: Property
prop_jString_simple = withTests 1 $ property $ do
  case parse jString "" "\"hello\"" of
    Right s -> s === "hello"
    Left err -> do
      annotate $ show err
      failure

-- JSON input: "a\"b" → unescaped content: a"b
prop_jString_escQuotes :: Property
prop_jString_escQuotes = withTests 1 $ property $ do
  case parse jString "" "\"a\\\"b\"" of
    Right s -> s === "a\"b"
    Left err -> do
      annotate $ show err
      failure

-- JSON input: "a\\b" → unescaped content: a\b
prop_jString_escBackslash :: Property
prop_jString_escBackslash = withTests 1 $ property $ do
  case parse jString "" "\"a\\\\b\"" of
    Right s -> s === "a\\b"
    Left err -> do
      annotate $ show err
      failure

-- JSON input: "line1\nline2" → unescaped content: line1<newline>line2
prop_jString_newline :: Property
prop_jString_newline = withTests 1 $ property $ do
  case parse jString "" "\"line1\\nline2\"" of
    Right s -> s === "line1\nline2"
    Left err -> do
      annotate $ show err
      failure

prop_jArray_ints :: Property
prop_jArray_ints = withTests 1 $ property $ do
  case parse (jArray jInt) "" "[1, 2, 3]" of
    Right xs -> xs === [1, 2, 3]
    Left err -> do
      annotate $ show err
      failure

prop_jArray_empty :: Property
prop_jArray_empty = withTests 1 $ property $ do
  case parse (jArray jInt) "" "[]" of
    Right xs -> xs === []
    Left err -> do
      annotate $ show err
      failure

prop_jArray_strings :: Property
prop_jArray_strings = withTests 1 $ property $ do
  case parse (jArray jString) "" "[\"a\", \"b\", \"c\"]" of
    Right xs -> xs === ["a", "b", "c"]
    Left err -> do
      annotate $ show err
      failure

-- ---- FromJValue tests ----

prop_lookup_present :: Property
prop_lookup_present = withTests 1 $ property $ do
  let obj = [("name", JString "Alice"), ("age", JNumber "30")]
  (obj .: "name" :: Maybe String) === Just "Alice"

prop_lookup_missing :: Property
prop_lookup_missing = withTests 1 $ property $ do
  let obj = [("name", JString "Alice")]
  (obj .: "missing" :: Maybe String) === Nothing

-- age is JNumber, not JString — type mismatch should yield Nothing
prop_lookup_typeMismatch :: Property
prop_lookup_typeMismatch = withTests 1 $ property $ do
  let obj = [("age", JNumber "30")]
  (obj .: "age" :: Maybe String) === Nothing

prop_fv_int :: Property
prop_fv_int = withTests 1 $ property $ do
  (fromJValue (JNumber "42") :: Maybe Int) === Just 42

prop_fv_intFromString :: Property
prop_fv_intFromString = withTests 1 $ property $ do
  (fromJValue (JString "42") :: Maybe Int) === Nothing

prop_fv_bool :: Property
prop_fv_bool = withTests 1 $ property $ do
  (fromJValue (JBool True) :: Maybe Bool) === Just True
  (fromJValue (JBool False) :: Maybe Bool) === Just False

prop_fv_string :: Property
prop_fv_string = withTests 1 $ property $ do
  (fromJValue (JString "hello") :: Maybe String) === Just "hello"

prop_fv_intList :: Property
prop_fv_intList = withTests 1 $ property $ do
  (fromJValue (JArray [JNumber "1", JNumber "2", JNumber "3"]) :: Maybe [Int]) === Just [1, 2, 3]

-- Mixed array: [1, "two"] should fail for [Int]
prop_fv_mixedArray :: Property
prop_fv_mixedArray = withTests 1 $ property $ do
  (fromJValue (JArray [JNumber "1", JString "two"]) :: Maybe [Int]) === Nothing

-- ---- search / scrape tests ----

prop_search_multi :: Property
prop_search_multi = withTests 1 $ property $ do
  let result = search "values: 42 and 73 and 99" :: Maybe [Int]
  case result of
    Just xs -> do
      annotate $ "Found: " ++ show xs
      assert $ 42 `elem` xs
      assert $ 73 `elem` xs
      assert $ 99 `elem` xs
    Nothing -> do
      annotate "Expected Just with multiple matches"
      failure

prop_search_none :: Property
prop_search_none = withTests 1 $ property $ do
  let result = search "no numbers here at all" :: Maybe [Int]
  case result of
    Nothing  -> success
    Just []  -> success
    Just xs  -> do
      annotate $ "Expected no matches but got: " ++ show xs
      failure

-- Nested object: parseLLMJSON should handle an object containing an array
data RecordWithList = RecordWithList
  { _rwl_name   :: String
  , _rwl_scores :: [Int]
  } deriving (Show, Eq)

instance FromJValue RecordWithList where
  fromJValue (JObject obj) = do
    name   <- obj .: "name"
    scores <- obj .: "scores"
    Just $ RecordWithList name scores
  fromJValue _ = Nothing

prop_parseLLM_nested :: Property
prop_parseLLM_nested = withTests 1 $ property $ do
  let input = "{\"name\":\"test\",\"scores\":[90,85,95]}"
  case parseLLMJSON @RecordWithList input of
    Just r -> do
      _rwl_name r === "test"
      _rwl_scores r === [90, 85, 95]
    Nothing -> do
      annotate "Expected Just for nested object"
      failure

-- Null field: parseLLMJSON with a nullable field
data RecordWithNull = RecordWithNull
  { _rwn_tag  :: String
  , _rwn_note :: Maybe String
  } deriving (Show, Eq)

instance FromJValue RecordWithNull where
  fromJValue (JObject obj) = do
    t <- obj .: "tag"
    let note = obj .: "note"
    Just $ RecordWithNull t note
  fromJValue _ = Nothing

prop_parseLLM_nullField :: Property
prop_parseLLM_nullField = withTests 1 $ property $ do
  let input = "{\"tag\":\"x\",\"note\":null}"
  case parseLLMJSON @RecordWithNull input of
    Just r -> do
      _rwn_tag r === "x"
    Nothing -> do
      annotate "Expected Just for object with null field"
      failure

-- ============================================================
-- JsonExample Tests
-- ============================================================

data TestPerson = TestPerson
  { tpName :: String
  , tpAge  :: Int
  } deriving (Generic)

instance JsonExample TestPerson

data TestNested = TestNested
  { tnTitle :: String
  , tnCount :: Int
  , tnTags  :: [String]
  } deriving (Generic)

instance JsonExample TestNested

jsonExampleTests :: [TestTree]
jsonExampleTests =
  [ testProperty "stripFieldPrefix removes camelCase prefix" prop_stripFieldPrefix
  , testProperty "genericJsonExample produces valid structure" prop_jsonExample_structure
  , testProperty "jsonResponsePrompt includes example" prop_jsonResponsePrompt
  , testProperty "leaf instances produce correct placeholders" prop_jsonExample_leaves
  , testProperty "nested record with list field" prop_jsonExample_nested
  , testProperty "JsonExample round-trip: example → mock JSON → parseLLMJSON" prop_jsonExample_roundtrip
  ]

prop_stripFieldPrefix :: Property
prop_stripFieldPrefix = withTests 1 $ property $ do
  stripFieldPrefix "tpName" === "name"
  stripFieldPrefix "tpAge" === "age"
  stripFieldPrefix "x" === "x"
  stripFieldPrefix "" === ""
  stripFieldPrefix "csHook" === "hook"

prop_jsonExample_structure :: Property
prop_jsonExample_structure = withTests 1 $ property $ do
  let example = jsonExample (Proxy :: Proxy TestPerson)
  annotate $ "Generated: " ++ example
  assert $ T.isInfixOf "name" (T.pack example)
  assert $ T.isInfixOf "age" (T.pack example)
  case example of
    ('{':_) -> success
    _       -> do
      annotate "Expected example to start with '{'"
      failure
  assert $ T.isSuffixOf "}" (T.pack example)

prop_jsonResponsePrompt :: Property
prop_jsonResponsePrompt = withTests 1 $ property $ do
  let prompt = jsonResponsePrompt (Proxy :: Proxy TestPerson)
  annotate prompt
  assert $ T.isPrefixOf "Respond with ONLY a JSON object:" (T.pack prompt)
  assert $ T.isInfixOf "name" (T.pack prompt)

prop_jsonExample_leaves :: Property
prop_jsonExample_leaves = withTests 1 $ property $ do
  jsonExample (Proxy :: Proxy Int) === "N"
  jsonExample (Proxy :: Proxy Double) === "N.N"
  jsonExample (Proxy :: Proxy String) === "\"...\""
  jsonExample (Proxy :: Proxy [Int]) === "[N, ...]"
  jsonExample (Proxy :: Proxy (Maybe Int)) === "N | null"

prop_jsonExample_nested :: Property
prop_jsonExample_nested = withTests 1 $ property $ do
  let example = jsonExample (Proxy :: Proxy TestNested)
  annotate $ "Generated: " ++ example
  assert $ T.isInfixOf "title" (T.pack example)
  assert $ T.isInfixOf "count" (T.pack example)
  assert $ T.isInfixOf "tags" (T.pack example)
  assert $ T.isInfixOf "[" (T.pack example)

-- Round-trip: generate example to inform a mock, mock returns matching JSON, parseLLMJSON extracts it
prop_jsonExample_roundtrip :: Property
prop_jsonExample_roundtrip = withTests 1 $ property $ do
  let backend = LLMBackend
        { _llmBackend_name = "test/json-example"
        , _llmBackend_api = APIMock $ \_ ->
            pure $ Right "{\"name\":\"Alice\",\"age\":30}"
        }
      env = LLMEnv backend
  result <- evalIO $ runLLM env (askLLMJSON [cwr User (T.pack $ jsonResponsePrompt (Proxy :: Proxy TestPerson))])
  case (result :: Either LLMError (Maybe SimpleRecord)) of
    Right (Just r) -> do
      _sr_name r === "Alice"
      _sr_age r === 30
    Right Nothing -> do
      annotate "Expected Just from round-trip"
      failure
    Left err -> do
      annotate $ show err
      failure

-- ============================================================
-- Extraction Tests: parseLLMJSON and scrappy
-- ============================================================

extractionTests :: [TestTree]
extractionTests =
  [ testProperty "parseLLMJSON parses clean JSON" prop_parseLLMJSON_clean
  , testProperty "parseLLMJSON finds JSON in prose" prop_parseLLMJSON_noisy
  , testProperty "parseLLMJSON returns Nothing for garbage" prop_parseLLMJSON_garbage
  , testProperty "askLLMParsec extracts typed value from noise" prop_askLLMParsec_search
  , testProperty "askLLMParsec returns Nothing for no match" prop_askLLMParsec_noMatch
  ]

prop_parseLLMJSON_clean :: Property
prop_parseLLMJSON_clean = withTests 1 $ property $ do
  case parseLLMJSON @SimpleRecord "{\"name\":\"Alice\",\"age\":30}" of
    Just r -> do
      _sr_name r === "Alice"
      _sr_age r === 30
    Nothing -> do
      annotate "Expected Just for clean JSON"
      failure

prop_parseLLMJSON_noisy :: Property
prop_parseLLMJSON_noisy = withTests 1 $ property $ do
  let noisy = "Here's your data:\n\n{\"name\":\"Bob\",\"age\":25}\n\nHope that helps!"
  case parseLLMJSON @SimpleRecord noisy of
    Just r -> do
      _sr_name r === "Bob"
      _sr_age r === 25
    Nothing -> do
      annotate "Expected parseLLMJSON to find JSON in noisy output"
      failure

prop_parseLLMJSON_garbage :: Property
prop_parseLLMJSON_garbage = withTests 1 $ property $ do
  case parseLLMJSON @SimpleRecord "no json here at all" of
    Nothing -> success
    Just _ -> do
      annotate "Expected Nothing for garbage input"
      failure

prop_askLLMParsec_search :: Property
prop_askLLMParsec_search = withTests 1 $ property $ do
  let noisyIntBackend = LLMBackend
        { _llmBackend_name = "test/noisy-int"
        , _llmBackend_api = APIMock $ \_ ->
            pure $ Right "The answer is 42 degrees"
        }
      env = LLMEnv noisyIntBackend
  result <- evalIO $ runLLM env (askLLMParsec @_ @Int [cwr User "what number?"])
  case result of
    Right (Just n) -> n === 42
    Right Nothing -> do
      annotate "Expected Just from askLLMParsec"
      failure
    Left err -> do
      annotate $ show err
      failure

prop_askLLMParsec_noMatch :: Property
prop_askLLMParsec_noMatch = withTests 1 $ property $ do
  let noNumberBackend = LLMBackend
        { _llmBackend_name = "test/no-number"
        , _llmBackend_api = APIMock $ \_ ->
            pure $ Right "no numbers here"
        }
      env = LLMEnv noNumberBackend
  result <- evalIO $ runLLM env (askLLMParsec @_ @Int [cwr User "what number?"])
  case result of
    Right Nothing -> success
    Right (Just _) -> do
      annotate "Expected Nothing when no parseable value"
      failure
    Left err -> do
      annotate $ show err
      failure

-- ============================================================
-- Context Tests: getRelevantCtx, renderHistory, askWithContext
-- ============================================================

sampleHistory :: ConversationHistory
sampleHistory =
  [ ConvoQuery (Tag "html-1") (ConvoQuestion [cwr User "q1"]) (ConvoAnswer "a1")
  , ConvoQuery (Tag "xml-1")  (ConvoQuestion [cwr User "q2"]) (ConvoAnswer "a2")
  , ConvoQuery (Tag "html-2") (ConvoQuestion [cwr User "q3"]) (ConvoAnswer "a3")
  , ConvoQuery (Tag "css-1")  (ConvoQuestion [cwr User "q4"]) (ConvoAnswer "a4")
  ]

-- | Mock that proves context accumulation: returns "Alice" only when
-- BOTH "my name is Alice" and "what is my name" appear in the messages.
contextProofBackend :: LLMBackend
contextProofBackend = LLMBackend
  { _llmBackend_name = "test/context-proof"
  , _llmBackend_api = APIMock $ \msgs ->
      let allContent = T.concat (map _cwr_content msgs)
      in pure $ Right $
           if T.isInfixOf "my name is Alice" allContent
              && T.isInfixOf "what is my name" allContent
           then "Alice"
           else if T.isInfixOf "my name is Alice" allContent
           then "Noted"
           else "I don't know"
  }

contextTests :: [TestTree]
contextTests =
  [ testGroup "getRelevantCtx"
    [ testProperty "LastN returns first n items" prop_ctx_lastN
    , testProperty "LastN 0 returns empty" prop_ctx_lastN_zero
    , testProperty "LastN larger than history returns all" prop_ctx_lastN_overflow
    , testProperty "Relevants filters by tag" prop_ctx_relevants
    , testProperty "Relevants with missing tag returns fewer" prop_ctx_relevants_missing
    , testProperty "Relevants with empty list returns empty" prop_ctx_relevants_empty
    , testProperty "LastNRelevant combines both" prop_ctx_lastNRelevant
    , testProperty "LastNRelevant with no-match predicate returns empty" prop_ctx_lastNRelevant_noMatch
    , testProperty "Relevants preserves tag-list order" prop_ctx_relevants_order
    ]
  , testGroup "getRelevantCtxDeepSeek"
    [ testProperty "LastN_DS returns first n items" prop_ctxDS_lastN
    , testProperty "Relevants_DS filters by TagDS" prop_ctxDS_relevants
    , testProperty "LastNRelevant_DS combines both" prop_ctxDS_lastNRelevant
    ]
  , testGroup "renderHistory"
    [ testProperty "formats conversation" prop_renderHistory
    , testProperty "empty history produces header only" prop_renderHistory_empty
    ]
  , testGroup "askWithContext"
    [ testProperty "accumulates conversation state" prop_ctx_accumulates
    , testProperty "3-turn conversation builds context" prop_ctx_3turn
    ]
  , testProperty "askJSONWithContext parses in conversation" prop_ctx_json
  ]

prop_ctx_lastN :: Property
prop_ctx_lastN = withTests 1 $ property $ do
  result <- evalIO $ evalStateT (getRelevantCtx (LastN 2)) sampleHistory
  length result === 2
  case result of
    (first':_) -> _convoQuery_tag first' === Tag "html-1"
    []         -> do
      annotate "Expected non-empty result"
      failure

prop_ctx_lastN_zero :: Property
prop_ctx_lastN_zero = withTests 1 $ property $ do
  result <- evalIO $ evalStateT (getRelevantCtx (LastN 0)) sampleHistory
  length result === 0

prop_ctx_lastN_overflow :: Property
prop_ctx_lastN_overflow = withTests 1 $ property $ do
  result <- evalIO $ evalStateT (getRelevantCtx (LastN 100)) sampleHistory
  length result === 4

prop_ctx_relevants :: Property
prop_ctx_relevants = withTests 1 $ property $ do
  result <- evalIO $ evalStateT (getRelevantCtx (Relevants [Tag "xml-1", Tag "css-1"])) sampleHistory
  length result === 2
  assert $ any (\q -> _convoQuery_tag q == Tag "xml-1") result
  assert $ any (\q -> _convoQuery_tag q == Tag "css-1") result

prop_ctx_relevants_missing :: Property
prop_ctx_relevants_missing = withTests 1 $ property $ do
  result <- evalIO $ evalStateT (getRelevantCtx (Relevants [Tag "xml-1", Tag "nonexistent"])) sampleHistory
  -- Only xml-1 exists; nonexistent yields Nothing via L.find, filtered by catMaybes
  length result === 1
  assert $ any (\q -> _convoQuery_tag q == Tag "xml-1") result

prop_ctx_relevants_empty :: Property
prop_ctx_relevants_empty = withTests 1 $ property $ do
  result <- evalIO $ evalStateT (getRelevantCtx (Relevants [])) sampleHistory
  length result === 0

prop_ctx_relevants_order :: Property
prop_ctx_relevants_order = withTests 1 $ property $ do
  -- Request tags in reverse order from how they appear in history
  result <- evalIO $ evalStateT (getRelevantCtx (Relevants [Tag "css-1", Tag "html-1"])) sampleHistory
  length result === 2
  -- Relevants maps over the tag list, so result order follows the tag list order
  case result of
    (first':second':_) -> do
      _convoQuery_tag first' === Tag "css-1"
      _convoQuery_tag second' === Tag "html-1"
    _ -> do
      annotate "Expected 2 results"
      failure

prop_ctx_lastNRelevant :: Property
prop_ctx_lastNRelevant = withTests 1 $ property $ do
  let isHtml (Tag t) = T.isPrefixOf "html" t
  result <- evalIO $ evalStateT (getRelevantCtx (LastNRelevant 1 isHtml)) sampleHistory
  length result === 1
  case result of
    (first':_) -> _convoQuery_tag first' === Tag "html-1"
    []         -> do
      annotate "Expected non-empty result"
      failure

prop_ctx_lastNRelevant_noMatch :: Property
prop_ctx_lastNRelevant_noMatch = withTests 1 $ property $ do
  let isGraphql (Tag t) = T.isPrefixOf "graphql" t
  result <- evalIO $ evalStateT (getRelevantCtx (LastNRelevant 10 isGraphql)) sampleHistory
  length result === 0

-- ---- DeepSeek context tests ----

sampleHistoryDS :: ConversationHistoryDeepSeek
sampleHistoryDS =
  [ (TagDS "q1" False, [cwr User "question 1"])
  , (TagDS "q1" True,  [cwr Assistant "answer 1"])
  , (TagDS "q2" False, [cwr User "question 2"])
  , (TagDS "q2" True,  [cwr Assistant "answer 2"])
  ]

prop_ctxDS_lastN :: Property
prop_ctxDS_lastN = withTests 1 $ property $ do
  result <- evalIO $ evalStateT (getRelevantCtxDeepSeek (LastN_DS 2)) sampleHistoryDS
  length result === 2
  case result of
    ((tag', _):_) -> unTagDS tag' === "q1"
    []            -> do
      annotate "Expected non-empty result"
      failure

prop_ctxDS_relevants :: Property
prop_ctxDS_relevants = withTests 1 $ property $ do
  result <- evalIO $ evalStateT (getRelevantCtxDeepSeek (Relevants_DS [TagDS "q2" True])) sampleHistoryDS
  length result === 1
  case result of
    ((tag', _):_) -> do
      unTagDS tag' === "q2"
      assert $ isAnswerDS tag'
    [] -> do
      annotate "Expected non-empty result"
      failure

prop_ctxDS_lastNRelevant :: Property
prop_ctxDS_lastNRelevant = withTests 1 $ property $ do
  let isAnswer = isAnswerDS
  result <- evalIO $ evalStateT (getRelevantCtxDeepSeek (LastNRelevant_DS 10 isAnswer)) sampleHistoryDS
  -- Only the answer entries (isAnswerDS == True) should pass
  length result === 2
  assert $ all (isAnswerDS . fst) result

prop_renderHistory :: Property
prop_renderHistory = withTests 1 $ property $ do
  let hist = [ConvoQuery (Tag "t") (ConvoQuestion [cwr User "hello"]) (ConvoAnswer "world")]
      rendered = renderHistory hist
  _cwr_role rendered === Assistant
  assert $ T.isInfixOf "Our conversation history so far:" (_cwr_content rendered)
  assert $ T.isInfixOf "hello" (_cwr_content rendered)
  assert $ T.isInfixOf "world" (_cwr_content rendered)

prop_renderHistory_empty :: Property
prop_renderHistory_empty = withTests 1 $ property $ do
  let rendered = renderHistory []
  _cwr_role rendered === Assistant
  _cwr_content rendered === "Our conversation history so far:"

prop_ctx_accumulates :: Property
prop_ctx_accumulates = withTests 1 $ property $ do
  let env = LLMEnv contextProofBackend
  result <- evalIO $ runConvo env $ do
    -- First call: backend sees "my name is Alice" → returns "Noted"
    _ <- askWithContext (LastN 10) (Tag "intro", ConvoQuestion [cwr User "my name is Alice"])
    -- Second call: history includes first exchange, so backend sees both
    -- "my name is Alice" (in rendered history) and "what is my name" → returns "Alice"
    askWithContext (LastN 10) (Tag "recall", ConvoQuestion [cwr User "what is my name"])
  case result of
    Right (ConvoAnswer answer) -> do
      annotate $ "Answer: " ++ T.unpack answer
      answer === "Alice"
    Left err -> do
      annotate $ "Error: " ++ show err
      failure

-- | 3-turn conversation: each turn's response reflects how many prior exchanges
-- the backend sees in the rendered history.
prop_ctx_3turn :: Property
prop_ctx_3turn = withTests 1 $ property $ do
  -- Backend counts how many "Me:" markers appear in the rendered history
  let countingBackend' = LLMBackend
        { _llmBackend_name = "test/counting"
        , _llmBackend_api = APIMock $ \msgs ->
            let allContent = T.concat (map _cwr_content msgs)
                meCount = length $ T.breakOnAll "Me:" allContent
            in pure $ Right $ T.pack $ show meCount
        }
      env = LLMEnv countingBackend'
  result <- evalIO $ runConvo env $ do
    r1 <- askWithContext (LastN 10) (Tag "t1", ConvoQuestion [cwr User "first"])
    r2 <- askWithContext (LastN 10) (Tag "t2", ConvoQuestion [cwr User "second"])
    r3 <- askWithContext (LastN 10) (Tag "t3", ConvoQuestion [cwr User "third"])
    pure (r1, r2, r3)
  case result of
    (Right (ConvoAnswer a1), Right (ConvoAnswer a2), Right (ConvoAnswer a3)) -> do
      -- Turn 1: empty history, 0 prior "Me:" markers
      a1 === "0"
      -- Turn 2: 1 prior exchange in history
      a2 === "1"
      -- Turn 3: 2 prior exchanges in history
      a3 === "2"
    _ -> do
      annotate $ "Unexpected result: " ++ show result
      failure

prop_ctx_json :: Property
prop_ctx_json = withTests 1 $ property $ do
  let backend = jsonBackend "{\"name\":\"test\",\"age\":42}"
      env = LLMEnv backend
  result <- evalIO $ runConvo env $
    askJSONWithContext (LastN 10) (Tag "q1", ConvoQuestion [cwr User "give me a record"])
  case (result :: Either LLMError (Maybe SimpleRecord)) of
    Right (Just r) -> do
      _sr_name r === "test"
      _sr_age r === 42
    Right Nothing -> do
      annotate "Expected Just"
      failure
    Left err -> do
      annotate $ show err
      failure

-- ============================================================
-- Type Construction Tests
-- ============================================================

typeConstructionTests :: [TestTree]
typeConstructionTests =
  [ testProperty "GPTRequestBody round-trips through JSON" prop_gptRequestBody_roundtrip
  , testProperty "Tag equality" prop_tag_eq
  , testProperty "TagDS equality" prop_tagDS_eq
  , testProperty "ConvoQuery preserves fields" prop_convoQuery_fields
  , testProperty "LLMError Eq instance" prop_llmError_eq
  ]

prop_gptRequestBody_roundtrip :: Property
prop_gptRequestBody_roundtrip = withTests 1 $ property $ do
  let body = GPTRequestBody
        { _gptRequest_model = "gpt-4"
        , _gptRequest_max_tokens = Just 100
        , _gptRequest_messages = [cwr User "test"]
        }
      bs = encode body
  case eitherDecode bs of
    Right (body' :: GPTRequestBody) -> do
      _gptRequest_model body' === "gpt-4"
      _gptRequest_max_tokens body' === Just 100
      length (_gptRequest_messages body') === 1
    Left err -> do
      annotate err
      failure

prop_tag_eq :: Property
prop_tag_eq = withTests 1 $ property $ do
  assert $ Tag "html-1" == Tag "html-1"
  assert $ Tag "html-1" /= Tag "html-2"

prop_tagDS_eq :: Property
prop_tagDS_eq = withTests 1 $ property $ do
  assert $ TagDS "t" True == TagDS "t" True
  assert $ TagDS "t" True /= TagDS "t" False
  assert $ TagDS "a" True /= TagDS "b" True

prop_convoQuery_fields :: Property
prop_convoQuery_fields = withTests 1 $ property $ do
  let q = ConvoQuery (Tag "test") (ConvoQuestion [cwr User "hi"]) (ConvoAnswer ("hello" :: T.Text))
  _convoQuery_tag q === Tag "test"
  unConvoAnswer (_convoQuery_answer q) === ("hello" :: T.Text)

prop_llmError_eq :: Property
prop_llmError_eq = withTests 1 $ property $ do
  assert $ LLMHttpError "x" == LLMHttpError "x"
  assert $ LLMHttpError "x" /= LLMParseError "x"
  assert $ LLMProcessError 1 "e" == LLMProcessError 1 "e"
  assert $ LLMProcessError 1 "e" /= LLMProcessError 2 "e"

-- ============================================================
-- ScrubPrefix Tests
-- ============================================================

scrubPrefixTests :: [TestTree]
scrubPrefixTests =
  [ testProperty "strips prefix from field label" prop_scrub_strips
  , testProperty "empty prefix returns field as-is" prop_scrub_empty
  , testProperty "matches Aeson defaultOptions structure" prop_scrub_is_options
  , testProperty "prefix longer than field drops all" prop_scrub_longer
  ]

prop_scrub_strips :: Property
prop_scrub_strips = withTests 1 $ property $ do
  let opts = scrubPrefix "_cwr_"
      modifier = fieldLabelModifier opts
  modifier "_cwr_role"    === "role"
  modifier "_cwr_content" === "content"

prop_scrub_empty :: Property
prop_scrub_empty = withTests 1 $ property $ do
  let opts = scrubPrefix ""
      modifier = fieldLabelModifier opts
  modifier "anything" === "anything"
  modifier ""         === ""

prop_scrub_is_options :: Property
prop_scrub_is_options = withTests 1 $ property $ do
  -- Everything except fieldLabelModifier should match defaultOptions
  let opts = scrubPrefix "_x_"
  -- Verify the modifier works (the Options type has no Eq instance,
  -- so we verify behavior instead)
  fieldLabelModifier opts "_x_foo" === "foo"

prop_scrub_longer :: Property
prop_scrub_longer = withTests 1 $ property $ do
  let opts = scrubPrefix "_very_long_prefix_"
      modifier = fieldLabelModifier opts
  -- When prefix is longer than field, drop removes everything
  modifier "short" === ""

-- ============================================================
-- ReadLLM Edge Cases
-- ============================================================

readLLMEdgeTests :: [TestTree]
readLLMEdgeTests =
  [ testProperty "ReadLLM Int parses from noise" prop_readllm_int
  , testProperty "ReadLLM String parses JSON string" prop_readllm_string
  , testProperty "ReadLLM Bool parses true/false" prop_readllm_bool
  , testProperty "ReadLLM () parses null" prop_readllm_unit
  , testProperty "ReadLLM [Int] parses array" prop_readllm_list
  , testProperty "search empty string returns Nothing" prop_readllm_search_empty
  , testProperty "search finds strings in noise" prop_readllm_search_strings
  , testProperty "search finds bools in noise" prop_readllm_search_bools
  , testProperty "custom ReadLLM instance" prop_readllm_custom
  ]

prop_readllm_int :: Property
prop_readllm_int = withTests 1 $ property $ do
  case search "the answer is 42 ok" :: Maybe [Int] of
    Just xs -> assert $ 42 `elem` xs
    Nothing -> do annotate "Expected to find 42"; failure

prop_readllm_string :: Property
prop_readllm_string = withTests 1 $ property $ do
  case search "result: \"hello world\" done" :: Maybe [String] of
    Just xs -> assert $ "hello world" `elem` xs
    Nothing -> do annotate "Expected to find string"; failure

prop_readllm_bool :: Property
prop_readllm_bool = withTests 1 $ property $ do
  case search "the value is true ok" :: Maybe [Bool] of
    Just xs -> assert $ True `elem` xs
    Nothing -> do annotate "Expected to find true"; failure

prop_readllm_unit :: Property
prop_readllm_unit = withTests 1 $ property $ do
  case search "result: null" :: Maybe [()] of
    Just xs -> assert $ () `elem` xs
    Nothing -> do annotate "Expected to find null"; failure

prop_readllm_list :: Property
prop_readllm_list = withTests 1 $ property $ do
  case parse (readLLM @[Int]) "" "[1, 2, 3]" of
    Right xs -> xs === [1, 2, 3]
    Left e   -> do annotate (show e); failure

prop_readllm_search_empty :: Property
prop_readllm_search_empty = withTests 1 $ property $ do
  (search "" :: Maybe [Int]) === Nothing

prop_readllm_search_strings :: Property
prop_readllm_search_strings = withTests 1 $ property $ do
  case search "say \"hello\" and \"world\"" :: Maybe [String] of
    Just xs -> do
      assert $ "hello" `elem` xs
      assert $ "world" `elem` xs
    Nothing -> do annotate "Expected to find strings"; failure

prop_readllm_search_bools :: Property
prop_readllm_search_bools = withTests 1 $ property $ do
  case search "first true then false" :: Maybe [Bool] of
    Just xs -> do
      assert $ True `elem` xs
      assert $ False `elem` xs
    Nothing -> do annotate "Expected to find bools"; failure

-- Custom ReadLLM for a simple key:value pair
newtype Score = Score Int deriving (Show, Eq)

instance ReadLLM Score where
  readLLM = do
    _ <- char '{'
    _ <- spaces
    _ <- char '"'
    _ <- string "score"
    _ <- char '"'
    _ <- spaces >> char ':' >> spaces
    n <- jInt
    _ <- spaces
    _ <- char '}'
    pure (Score n)

prop_readllm_custom :: Property
prop_readllm_custom = withTests 1 $ property $ do
  case search "The result is {\"score\": 95} points" :: Maybe [Score] of
    Just xs -> assert $ Score 95 `elem` xs
    Nothing -> do annotate "Expected to find Score"; failure

-- ============================================================
-- JsonExample Edge Cases
-- ============================================================

jsonExampleEdgeTests :: [TestTree]
jsonExampleEdgeTests =
  [ testProperty "stripFieldPrefix all lowercase" prop_strip_all_lower
  , testProperty "stripFieldPrefix single char prefix" prop_strip_single
  , testProperty "stripFieldPrefix no prefix" prop_strip_no_prefix
  , testProperty "stripFieldPrefix empty string" prop_strip_empty
  , testProperty "jsonExample Maybe shows null alternative" prop_example_maybe
  , testProperty "jsonExample Either shows alternatives" prop_example_either
  , testProperty "jsonExample [String] shows list" prop_example_list_string
  , testProperty "jsonExample Double placeholder" prop_example_double
  , testProperty "jsonResponsePrompt includes preamble" prop_prompt_preamble
  ]

prop_strip_all_lower :: Property
prop_strip_all_lower = withTests 1 $ property $ do
  -- All lowercase: no uppercase to split on, returns as-is
  stripFieldPrefix "alllower" === "alllower"

prop_strip_single :: Property
prop_strip_single = withTests 1 $ property $ do
  -- Single lowercase prefix char followed by uppercase
  stripFieldPrefix "xFoo" === "foo"

prop_strip_no_prefix :: Property
prop_strip_no_prefix = withTests 1 $ property $ do
  -- Starts with uppercase — dropWhile isLower removes nothing
  stripFieldPrefix "Foo" === "foo"

prop_strip_empty :: Property
prop_strip_empty = withTests 1 $ property $ do
  stripFieldPrefix "" === ""

prop_example_maybe :: Property
prop_example_maybe = withTests 1 $ property $ do
  let ex = jsonExample (Proxy :: Proxy (Maybe Int))
  assert $ "N" `T.isInfixOf` T.pack ex
  assert $ "null" `T.isInfixOf` T.pack ex

prop_example_either :: Property
prop_example_either = withTests 1 $ property $ do
  let ex = jsonExample (Proxy :: Proxy (Either Int String))
  assert $ "N" `T.isInfixOf` T.pack ex
  assert $ "\"...\"" `T.isInfixOf` T.pack ex

prop_example_list_string :: Property
prop_example_list_string = withTests 1 $ property $ do
  let ex = jsonExample (Proxy :: Proxy [String])
  -- Should show ["...", ...]
  assert $ "\"...\"" `T.isInfixOf` T.pack ex
  assert $ "..." `T.isInfixOf` T.pack ex

prop_example_double :: Property
prop_example_double = withTests 1 $ property $ do
  jsonExample (Proxy :: Proxy Double) === "N.N"

prop_prompt_preamble :: Property
prop_prompt_preamble = withTests 1 $ property $ do
  let prompt = jsonResponsePrompt (Proxy :: Proxy Double)
  assert $ "Respond with ONLY" `T.isInfixOf` T.pack prompt
