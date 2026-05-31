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
import LLM.LLM (renderContextLabeled, prepareContext, getRelevantCtx, renderHistory)
import LLM.Provider
import LLM.Provider.Backends
import LLM.ReadLLM

import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.State (evalStateT, execStateT)
import Data.Functor.Identity (runIdentity)
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
  , testGroup "ArgMax" argMaxTests
  , testGroup "ContextLabeled" contextLabeledTests
  , testGroup "PrepareContext" prepareContextTests
  , testGroup "Integration" integrationTests
  , testGroup "TagPattern" tagPatternTests
  , testGroup "GetsContext" getsContextTests
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
  let env = mkLLMEnv echoBackend
      msgs = [cwr User txt]
  result <- evalIO $ runLLM env (askLLM msgs)
  result === Right txt

prop_askLLMJSON_parses :: Property
prop_askLLMJSON_parses = property $ do
  let env = mkLLMEnv (jsonBackend "{\"name\":\"Alice\",\"age\":30}")
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
  let env = mkLLMEnv (jsonBackend "this is not json")
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
      env = mkLLMEnv (failBackend err)
  result <- evalIO $ runLLM env (askLLM [cwr User "test"])
  case result of
    Left (LLMHttpError msg) -> msg === errMsg
    _ -> do
      annotate "Expected Left LLMHttpError"
      failure

prop_askLLMJSON_fenced :: Property
prop_askLLMJSON_fenced = withTests 1 $ property $ do
  let env = mkLLMEnv (jsonBackend "Sure! Here's your JSON:\n\n```json\n{\"name\":\"Bob\",\"age\":25}\n```\n")
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
claudeEnv = mkLLMEnv (mkClaudeCLI "haiku")

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
  let env = mkLLMEnv bookFieldBackend

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
  let env = mkLLMEnv noisyBookBackend

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
-- ARG_MAX Tests: pure checkArgMax
-- ============================================================

argMaxTests :: [TestTree]
argMaxTests =
  [ testProperty "checkArgMax allows small args under limit" prop_argMax_ok
  , testProperty "checkArgMax rejects args exceeding limit" prop_argMax_exceeded
  , testProperty "checkArgMax counts env vars toward total" prop_argMax_env_counted
  ]

prop_argMax_ok :: Property
prop_argMax_ok = withTests 1 $ property $ do
  let args = ["claude", "-p", "--model", "haiku", "hello world"]
      envVars = [("HOME", "/home/user"), ("PATH", "/usr/bin")]
  case checkArgMax 2097152 args envVars of
    Right () -> success
    Left (actual, limit) -> do
      annotate $ "Expected Right (), got Left (" ++ show actual ++ ", " ++ show limit ++ ")"
      failure

prop_argMax_exceeded :: Property
prop_argMax_exceeded = withTests 1 $ property $ do
  -- Create a very large arg that exceeds a tiny limit
  let bigArg = replicate 1000 'x'
      args = ["claude", bigArg]
      envVars = []
  case checkArgMax 500 args envVars of
    Left (actual, limit) -> do
      assert $ actual > limit
      limit === 500
    Right () -> do
      annotate "Expected Left, got Right"
      failure

prop_argMax_env_counted :: Property
prop_argMax_env_counted = withTests 1 $ property $ do
  let args = ["claude", "hi"]
      bigEnv = [(replicate 500 'K', replicate 500 'V')]
  -- With a 600-byte limit, the env alone (500+500+2 bytes for k=v\0) should exceed it
  case checkArgMax 600 args bigEnv of
    Left (actual, _) -> assert $ actual > 600
    Right () -> do
      annotate "Expected Left — env vars should push total over limit"
      failure

-- ============================================================
-- Context Labeled Tests: renderContextLabeled
-- ============================================================

contextLabeledTests :: [TestTree]
contextLabeledTests =
  [ testProperty "wraps output in <context> tags" prop_context_tags
  , testProperty "renders ConvoExchange as Me/Assistant" prop_context_exchange
  , testProperty "renders ConvoSummary with [Summary] prefix" prop_context_summary
  ]

-- Helper: build a ConvoExchange from question/answer text
mkExchange :: T.Text -> T.Text -> T.Text -> ConvoEntry T.Text
mkExchange tagTxt question answer =
  ConvoExchange $ ConvoQuery (Tag tagTxt) (ConvoQuestion [cwr User question]) (ConvoAnswer answer)

prop_context_tags :: Property
prop_context_tags = withTests 1 $ property $ do
  let hist = [mkExchange "tag1" "hello" "world"]
      rendered = _cwr_content $ renderContextLabeled hist
  assert $ T.isPrefixOf "<context>" rendered
  assert $ T.isSuffixOf "</context>" rendered

prop_context_exchange :: Property
prop_context_exchange = withTests 1 $ property $ do
  let hist = [mkExchange "tag1" "what color?" "blue"]
      rendered = _cwr_content $ renderContextLabeled hist
  assert $ T.isInfixOf "Assistant: blue" rendered

prop_context_summary :: Property
prop_context_summary = withTests 1 $ property $ do
  let hist = [ConvoSummary (Tag "Run0--Summary") "The subject is a red fox on a green background."]
      rendered = _cwr_content $ renderContextLabeled hist
  assert $ T.isInfixOf "[Summary of prior context]" rendered
  assert $ T.isInfixOf "red fox" rendered

-- ============================================================
-- PrepareContext Tests: mock backend
-- ============================================================

prepareContextTests :: [TestTree]
prepareContextTests =
  [ testProperty "no limit returns labeled context" prop_prepare_no_limit
  , testProperty "under-limit returns labeled context unchanged" prop_prepare_under_limit
  , testProperty "over-limit triggers summarization" prop_prepare_over_limit
  , testProperty "summarization failure falls back to truncation" prop_prepare_summarize_fails
  , testProperty "summary rebinds state with ConvoSummary" prop_prepare_rebinds_state
  ]

-- | Mock backend that echoes a summary for summarization calls
summaryMockBackend :: LLMBackend
summaryMockBackend = LLMBackend
  { _llmBackend_name = "test/summary-mock"
  , _llmBackend_api  = APIMock $ \msgs ->
      let totalLen = sum $ map (T.length . _cwr_content) msgs
      in pure $ Right $ "Color is blue. Canvas is 800x600. Subject is fox. (condensed from " <> T.pack (show totalLen) <> " chars)"
  }

-- | Mock backend that always fails (for testing summarization failure fallback)
summaryFailMockBackend :: LLMBackend
summaryFailMockBackend = LLMBackend
  { _llmBackend_name = "test/summary-fail"
  , _llmBackend_api  = APIMock $ \_ -> pure $ Left $ LLMProcessError 1 "mock summarization failure"
  }

-- | Seed history with enough entries to generate substantial context
seedHistory :: ConversationHistory
seedHistory =
  [ mkExchange "Run0--Classify" "Classify this: a spider walking" "{\"kind\":\"animation\",\"subject\":\"spider\",\"mood\":\"creepy\",\"keywords\":[\"spider\",\"web\",\"crawl\"]}"
  , mkExchange "Run0--Canvas" "Pick canvas for spider animation" "{\"width\":1080,\"height\":1920,\"bg\":\"1a1a2e\",\"fps\":30,\"duration\":3.0}"
  , mkExchange "Run0--Elements" "Design elements for spider" "{\"layers\":[{\"name\":\"body\",\"shape\":\"ellipse\"},{\"name\":\"legs\",\"shape\":\"path\"}]}"
  ]

prop_prepare_no_limit :: Property
prop_prepare_no_limit = withTests 1 $ property $ do
  let backend = LLMBackend "unused" (APIMock $ \_ -> pure $ Right "unused")
      env = LLMEnv backend Nothing
  result <- evalIO $ runReaderT (evalStateT (prepareContext (LastN 10) (Tag "test")) seedHistory) env
  case result of
    [msg] -> do
      assert $ T.isInfixOf "<context>" (_cwr_content msg)
      assert $ T.isInfixOf "</context>" (_cwr_content msg)
    other -> do
      annotate $ "Expected 1 message, got " ++ show (length other)
      failure

prop_prepare_under_limit :: Property
prop_prepare_under_limit = withTests 1 $ property $ do
  let backend = LLMBackend "unused" (APIMock $ \_ -> pure $ Right "unused")
      env = LLMEnv backend (Just 100000)
  result <- evalIO $ runReaderT (evalStateT (prepareContext (LastN 10) (Tag "test")) seedHistory) env
  case result of
    [msg] -> do
      -- Should return full context since it's under the 100k limit
      assert $ T.isInfixOf "<context>" (_cwr_content msg)
      assert $ T.isInfixOf "spider" (_cwr_content msg)
    other -> do
      annotate $ "Expected 1 message, got " ++ show (length other)
      failure

prop_prepare_over_limit :: Property
prop_prepare_over_limit = withTests 1 $ property $ do
  -- Set a very small limit to force summarization
  let env = LLMEnv summaryMockBackend (Just 50)
  result <- evalIO $ runReaderT (evalStateT (prepareContext (LastN 10) (Tag "test")) seedHistory) env
  case result of
    [msg] -> do
      -- Should contain the mock summary, not the original exchanges
      assert $ T.isInfixOf "condensed from" (_cwr_content msg)
    other -> do
      annotate $ "Expected 1 message, got " ++ show (length other)
      failure

prop_prepare_summarize_fails :: Property
prop_prepare_summarize_fails = withTests 1 $ property $ do
  let env = LLMEnv summaryFailMockBackend (Just 50)
  result <- evalIO $ runReaderT (evalStateT (prepareContext (LastN 10) (Tag "test")) seedHistory) env
  case result of
    [msg] -> do
      -- Should fall back to truncation
      assert $ T.isInfixOf "[...truncated]" (_cwr_content msg)
    other -> do
      annotate $ "Expected 1 message, got " ++ show (length other)
      failure

prop_prepare_rebinds_state :: Property
prop_prepare_rebinds_state = withTests 1 $ property $ do
  let env = LLMEnv summaryMockBackend (Just 50)
  newState <- evalIO $ runReaderT (execStateT (prepareContext (LastN 10) (Tag "test")) seedHistory) env
  -- After summarization, state should contain a ConvoSummary
  case newState of
    (ConvoSummary tag _summaryText : _rest) -> do
      -- Tag should contain "Summary"
      assert $ T.isInfixOf "Summary" (unTag tag)
      -- Tag should contain run parts from original entries
      assert $ T.isInfixOf "Run0" (unTag tag)
    _ -> do
      annotate $ "Expected ConvoSummary at head of state"
      failure

-- ============================================================
-- TagPattern Tests: matchTag + mkPattern
-- ============================================================

tagPatternTests :: [TestTree]
tagPatternTests =
  [ testProperty "exact match" prop_tag_exact_match
  , testProperty "wildcard matches any segment" prop_tag_wildcard
  , testProperty "case insensitive" prop_tag_case_insensitive
  , testProperty "rejects extra segments" prop_tag_rejects_extra_segments
  , testProperty "rejects fewer segments" prop_tag_rejects_fewer_segments
  , testProperty "leading wildcard" prop_tag_leading_wildcard
  , testProperty "all wildcards match any tag with same segment count" prop_tag_all_wildcards
  , testProperty "mkPattern splits on --" prop_mkPattern_splits
  ]

prop_tag_exact_match :: Property
prop_tag_exact_match = withTests 1 $ property $ do
  assert $ matchTag (mkPattern "Run0--Decomp") (Tag "Run0--Decomp")

prop_tag_wildcard :: Property
prop_tag_wildcard = withTests 1 $ property $ do
  let pat = mkPattern "Run0--PartDetail--*"
  assert $ matchTag pat (Tag "Run0--PartDetail--cat-legs")
  assert $ matchTag pat (Tag "Run0--PartDetail--bear-torso")
  assert $ not $ matchTag pat (Tag "Run0--Fill--cat-legs")

prop_tag_case_insensitive :: Property
prop_tag_case_insensitive = withTests 1 $ property $ do
  assert $ matchTag (mkPattern "run0--decomp") (Tag "Run0--Decomp")
  assert $ matchTag (mkPattern "RUN0--DECOMP") (Tag "run0--decomp")

prop_tag_rejects_extra_segments :: Property
prop_tag_rejects_extra_segments = withTests 1 $ property $ do
  assert $ not $ matchTag (mkPattern "Run0--Decomp") (Tag "Run0--Decomp--Extra")

prop_tag_rejects_fewer_segments :: Property
prop_tag_rejects_fewer_segments = withTests 1 $ property $ do
  assert $ not $ matchTag (mkPattern "*--*--*") (Tag "A--B")

prop_tag_leading_wildcard :: Property
prop_tag_leading_wildcard = withTests 1 $ property $ do
  let pat = mkPattern "*--Fill--ball-body"
  assert $ matchTag pat (Tag "Run0--Fill--ball-body")
  assert $ matchTag pat (Tag "Run5--Fill--ball-body")
  assert $ not $ matchTag pat (Tag "Run0--PartDetail--ball-body")

prop_tag_all_wildcards :: Property
prop_tag_all_wildcards = withTests 1 $ property $ do
  let pat = mkPattern "*--*"
  assert $ matchTag pat (Tag "Run0--Decomp")
  assert $ matchTag pat (Tag "anything--here")
  assert $ not $ matchTag pat (Tag "Run0--A--B")

prop_mkPattern_splits :: Property
prop_mkPattern_splits = withTests 1 $ property $ do
  unTagPattern (mkPattern "Run0--PartDetail--*") === ["Run0", "PartDetail", "*"]
  unTagPattern (mkPattern "Decomp") === ["Decomp"]
  unTagPattern (mkPattern "*--*--*") === ["*", "*", "*"]

-- ============================================================
-- GetsContext Tests: Gets + NoHistory in getRelevantCtx
-- ============================================================

-- | Simulated visual pipeline history: 3 structural + 15 PartDetail + 10 Fill = 28 entries
visualHistory :: ConversationHistory
visualHistory =
  [ mkExchange "Run0--Decomp" "decompose the scene" "{\"parts\":[\"body\",\"head\"]}"
  , mkExchange "Run0--Layout" "position the parts" "{\"parts\":[{\"name\":\"body\",\"x\":0.5}]}"
  , mkExchange "Run0--PartGuide" "reference guide" "{\"ready\":true}"
  ]
  ++ [ mkExchange ("Run0--PartDetail--part" <> T.pack (show i))
                  ("detail for part " <> T.pack (show i))
                  ("{\"name\":\"part" <> T.pack (show i) <> "\"}")
     | i <- [1..15 :: Int]
     ]
  ++ [ mkExchange ("Run0--Fill--part" <> T.pack (show i))
                  ("fill for part " <> T.pack (show i))
                  ("{\"color\":\"aa0000\"}")
     | i <- [1..10 :: Int]
     ]

runGets :: RelevantContext -> ConversationHistory -> ConversationHistory
runGets ctx hist = runIdentity $ evalStateT (getRelevantCtx ctx) hist

getsContextTests :: [TestTree]
getsContextTests =
  [ testProperty "pin returns all matches for exact tag" prop_gets_pin
  , testProperty "lastNMatching budgets correctly" prop_gets_lastN_budget
  , testProperty "multiple rules compose" prop_gets_compose
  , testProperty "chronological order preserved" prop_gets_order
  , testProperty "no matches returns empty" prop_gets_no_matches
  , testProperty "NoHistory returns empty" prop_gets_nohistory
  ]

prop_gets_pin :: Property
prop_gets_pin = withTests 1 $ property $ do
  let result = runGets (Gets [pin "Run0--Decomp"]) visualHistory
  length result === 1
  entryTag (Prelude.head result) === Tag "Run0--Decomp"

prop_gets_lastN_budget :: Property
prop_gets_lastN_budget = withTests 1 $ property $ do
  -- 15 PartDetail entries, budget of 5 → should get 5 most recent
  let result = runGets (Gets [lastNMatching 5 "Run0--PartDetail--*"]) visualHistory
  length result === 5
  -- Most recent means lowest indices in the list (history is newest-first)
  -- so the first 5 PartDetail matches
  let tags = map (unTag . entryTag) result
  assert $ all (T.isInfixOf "PartDetail") tags

prop_gets_compose :: Property
prop_gets_compose = withTests 1 $ property $ do
  let ctx = Gets
        [ pin "Run0--Decomp"
        , pin "Run0--Layout"
        , lastNMatching 3 "Run0--PartDetail--*"
        ]
      result = runGets ctx visualHistory
  -- 1 Decomp + 1 Layout + 3 PartDetail = 5
  length result === 5

prop_gets_order :: Property
prop_gets_order = withTests 1 $ property $ do
  let ctx = Gets
        [ pin "Run0--Decomp"
        , pin "Run0--Layout"
        , lastNMatching 2 "Run0--PartDetail--*"
        ]
      result = runGets ctx visualHistory
      tags = map (unTag . entryTag) result
  -- Decomp comes before Layout comes before PartDetails in the original history
  -- so the result should preserve that order
  let decompIdx = Prelude.head [i | (i, t) <- Prelude.zip [0..] tags, T.isInfixOf "Decomp" t]
      layoutIdx = Prelude.head [i | (i, t) <- Prelude.zip [0..] tags, T.isInfixOf "Layout" t]
      detailIdxs = [i | (i, t) <- Prelude.zip [0..] tags, T.isInfixOf "PartDetail" t]
  assert $ decompIdx < layoutIdx
  assert $ all (> layoutIdx) detailIdxs

prop_gets_no_matches :: Property
prop_gets_no_matches = withTests 1 $ property $ do
  let result = runGets (Gets [pin "Run99--Decomp"]) visualHistory
  length result === 0

prop_gets_nohistory :: Property
prop_gets_nohistory = withTests 1 $ property $ do
  let result = runGets NoHistory visualHistory
  length result === 0
