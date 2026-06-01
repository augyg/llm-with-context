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
  , catalogue
  , catalogueWith
  , compactSummarize
  , recallForPrompt
  , cataloguePrompt
  ) where

import qualified Data.Text as T

import Effectful (Eff, (:>))

import LLM.Brain.Key (canonicalizeTerms, keyOf)
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
