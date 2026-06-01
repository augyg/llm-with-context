{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Cross-effect combinators that bridge the 'LLM.Effect.LLM' effect and the
-- keyed 'Brain' — the write-time half of the BrainGraph design. Like
-- "LLM.Effect.Compaction", these live OUTSIDE the storage effect because they
-- need the LLM, and the dependency is visible in the constraints.
--
--   * 'catalogue' — LLM-at-write keying. The model reads a piece of text and
--     emits the entry's clean canonical @(nouns, verbs, adjectives)@ key plus a
--     self-contained markdown value; the terms are then lemmatised through the
--     same 'Lexicon' the read path uses, removing would-be noise at the source.
--   * 'compactSummarize' — the @LLMSummarize@ collision-merge strategy (cf. the
--     pure 'LLM.Brain.Store.compactConcat').
--   * 'recallForPrompt' — the cheap, LLM-free read path: key a prompt
--     deterministically and recall by overlap.
module LLM.Brain.Catalogue
  ( CatalogueResult (..)
  , PromptKeys (..)
  , catalogue
  , catalogueWith
  , compactSummarize
  , recallForPrompt
  , recallFocused
  , keyForPrompt
  , cataloguePrompt
  , keyExtractPrompt
  ) where

import qualified Data.Text as T

import Effectful (Eff, (:>))

import LLM.Brain.Key (BrainKey, canonicalizeTerms, keyOf)
import LLM.Brain.Lexicon (Lexicon)
import LLM.Brain.Store
  ( Brain
  , BrainEntry (..)
  , BrainId
  , RecallEffort
  , collisions
  , getEntry
  , recallByKey
  , rememberKeyed
  , replaceEntries
  )
import LLM.Effect (LLM, ask)
import LLM.Provider (parseLLMJSON)
import LLM.Types (ConvoError (..), GPTRole (System, User), cwr)

import Scrappy.JSON.Value (FromJValue (..), JValue (..))

-- | The cataloguer's structured reply: the entry's canonical key terms plus the
-- markdown value to store.
data CatalogueResult = CatalogueResult
  { crNouns :: [T.Text]
  , crVerbs :: [T.Text]
  , crAdjs  :: [T.Text]
  , crValue :: T.Text
  } deriving (Show)

instance FromJValue CatalogueResult where
  fromJValue (JObject obj) = do
    ns <- jField obj "nouns"
    vs <- jField obj "verbs"
    as <- jField obj "adjectives"
    v  <- jField obj "value"
    Just (CatalogueResult (map T.pack ns) (map T.pack vs) (map T.pack as) (T.pack v))
  fromJValue _ = Nothing

-- | Look up an object field and decode it (the same helper shape the pipeline
-- uses over scrappy-json).
jField :: FromJValue a => [(String, JValue)] -> String -> Maybe a
jField obj k = lookup k obj >>= fromJValue

-- | Catalogue text into a keyed brain entry using the default 'cataloguePrompt'.
-- Pin the provider: @catalogue \@'Anthropic someText@.
catalogue
  :: forall p es. (LLM p :> es, Brain :> es, Lexicon :> es)
  => T.Text -> Eff es (Either ConvoError BrainId)
catalogue = catalogueWith @p cataloguePrompt

-- | 'catalogue' with caller-supplied cataloguer instructions.
catalogueWith
  :: forall p es. (LLM p :> es, Brain :> es, Lexicon :> es)
  => T.Text -> T.Text -> Eff es (Either ConvoError BrainId)
catalogueWith instructions text = do
  res <- ask @p [cwr System instructions, cwr User text]
  case res of
    Left e -> pure (Left (ConvoError e))
    Right out -> case parseLLMJSON out :: Maybe CatalogueResult of
      Nothing -> pure (Left (ConvoError "catalogue: could not parse cataloguer JSON"))
      Just cr -> do
        key <- canonicalizeTerms (crNouns cr) (crVerbs cr) (crAdjs cr)
        i <- rememberKeyed key (crValue cr)
        pure (Right i)

-- | The @LLMSummarize@ collision-merge strategy: for each group of entries that
-- share an identical key, ask the provider to fuse their values into one note,
-- then 'replaceEntries'. A group whose summary call fails is left untouched.
-- Returns the number of groups merged. Pin the provider: @compactSummarize \@'Anthropic@.
compactSummarize
  :: forall p es. (LLM p :> es, Brain :> es)
  => Eff es Int
compactSummarize = do
  groups <- collisions
  merged <- mapM mergeGroup groups
  pure (length (filter id merged))
  where
    mergeGroup (_, ids) = do
      maybes <- mapM getEntry ids
      case [e | Just e <- maybes] of
        []           -> pure False
        (firstE : _) -> do
          let joined = T.intercalate "\n\n---\n\n" [beValue e | Just e <- maybes]
          res <- ask @p [cwr System summarizePrompt, cwr User joined]
          case res of
            Left _        -> pure False
            Right summary -> do
              _ <- replaceEntries ids (beKey firstE) summary
              pure True

-- | The cheap, LLM-free read path: key a prompt deterministically via the
-- 'Lexicon', then recall by weighted overlap + spreading activation.
recallForPrompt
  :: (Brain :> es, Lexicon :> es)
  => RecallEffort -> T.Text -> Eff es [BrainEntry]
recallForPrompt effort prompt = do
  key <- keyOf prompt
  recallByKey key effort

-- | The cataloguer's reply when keying a READ prompt: just the salient terms,
-- no value to store.
data PromptKeys = PromptKeys
  { pkNouns :: [T.Text]
  , pkVerbs :: [T.Text]
  , pkAdjs  :: [T.Text]
  } deriving (Show)

instance FromJValue PromptKeys where
  fromJValue (JObject obj) = do
    ns <- jField obj "nouns"
    vs <- jField obj "verbs"
    as <- jField obj "adjectives"
    Just (PromptKeys (map T.pack ns) (map T.pack vs) (map T.pack as))
  fromJValue _ = Nothing

-- | The FOCUSED read key: a pre-call asking provider @p@ to pick only the
-- salient keywords from a prompt and their part of speech, canonicalised through
-- the same 'Lexicon' the rest of the system uses. Where 'LLM.Brain.Key.keyOf'
-- keys every content word deterministically (cheap, but @"tell me about
-- butterflies"@ also keys @tell@), this lets the model focus the lookup on just
-- @Noun "butterfly"@. It is a plain combinator over the existing @LLM p@ 'ask'
-- (a normal API call with a key-extraction prompt) + the 'Lexicon' — no new
-- effect, mirroring 'LLM.Effect.Compaction.compact's @LLM p@+'Memory' shape.
keyForPrompt
  :: forall p es. (LLM p :> es, Lexicon :> es)
  => T.Text -> Eff es (Either ConvoError BrainKey)
keyForPrompt prompt = do
  res <- ask @p [cwr System keyExtractPrompt, cwr User prompt]
  case res of
    Left e -> pure (Left (ConvoError e))
    Right out -> case parseLLMJSON out :: Maybe PromptKeys of
      Nothing -> pure (Left (ConvoError "keyForPrompt: could not parse extracted keys"))
      Just pk -> Right <$> canonicalizeTerms (pkNouns pk) (pkVerbs pk) (pkAdjs pk)

-- | The focused read path: 'keyForPrompt' (LLM pre-call) then recall by that
-- key. The more "focused" alternative to 'recallForPrompt' — at the cost of one
-- LLM call per lookup. Pin the provider: @recallFocused \@'Anthropic effort prompt@.
recallFocused
  :: forall p es. (LLM p :> es, Brain :> es, Lexicon :> es)
  => RecallEffort -> T.Text -> Eff es (Either ConvoError [BrainEntry])
recallFocused effort prompt = do
  eKey <- keyForPrompt @p prompt
  case eKey of
    Left e    -> pure (Left e)
    Right key -> Right <$> recallByKey key effort

-- | Instructions for the focused key pre-call.
keyExtractPrompt :: T.Text
keyExtractPrompt = T.unlines
  [ "For the user prompt below, determine the few KEY search keywords a memory"
  , "lookup should focus on, and whether each is a noun, verb, or adjective."
  , "Return ONLY a JSON object of EXACTLY this shape:"
  , "{\"nouns\": [...], \"verbs\": [...], \"adjectives\": [...]}"
  , ""
  , "Include only salient content words (the actual topic); drop filler verbs like"
  , "tell / show / explain / give and all function words. Prefer 1-4 keywords total."
  , "Example: \"tell me about butterflies\" -> {\"nouns\": [\"butterflies\"], \"verbs\": [], \"adjectives\": []}"
  ]

-- | The default cataloguer instructions. The model must emit canonical key
-- terms (so write-vocabulary lines up with the read path) and a durable,
-- self-contained value.
cataloguePrompt :: T.Text
cataloguePrompt = T.unlines
  [ "You are a memory cataloguer for a symbolic index. Read the text and emit JSON that keys it for later retrieval."
  , "Return ONLY a JSON object of EXACTLY this shape:"
  , "{\"nouns\": [...], \"verbs\": [...], \"adjectives\": [...], \"value\": \"...\"}"
  , ""
  , "Rules:"
  , "- nouns / verbs / adjectives: the few KEY content words a future query would use to find this memory."
  , "  Canonical form ONLY: singular nouns, base-form verbs (no -ing/-ed/-s), no function words, no duplicates."
  , "  Prefer 2-6 nouns; include verbs/adjectives only when genuinely central to the memory."
  , "- value: a self-contained Markdown note (one '##' heading plus body) capturing the durable fact, decision,"
  , "  or guidance, written to be useful months later with no other context. Use [[term]] to reference related concepts."
  , "- If the text carries no durable memory, return empty arrays and an empty value string."
  ]

-- | Instructions for the 'LLMSummarize' merge.
summarizePrompt :: T.Text
summarizePrompt = T.unlines
  [ "These Markdown notes share the same index key and are being merged into ONE note."
  , "Produce a single self-contained Markdown note (one '##' heading plus body) that preserves EVERY distinct"
  , "fact, decision, and [[link]] across them, removes duplication, and reads coherently. Return ONLY the note."
  ]
