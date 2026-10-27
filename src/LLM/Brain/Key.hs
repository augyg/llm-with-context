{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | @BrainKey@ — the symbolic index key for a brain entry: the canonical noun,
-- verb and adjective lemma sets of a piece of text.
--
-- Two paths produce keys, and they MUST agree on vocabulary:
--
--   * 'keyOf' — the cheap, deterministic READ path. Tokenise a prompt, classify
--     and lemmatise each word via the 'Lexicon', and bucket it. A 'Both' word
--     (noun-or-verb) lands in /both/ noun and verb sets so it can match either.
--
--   * 'canonicalizeTerms' — the WRITE path. The cataloguer (an LLM, see
--     "LLM.Brain.Catalogue") has already chosen each term's bucket and stripped
--     noise, so we only lemmatise the terms through the /same/ 'Lexicon' — this
--     is what makes a written @scrape@ match a read @scraping@.
--
-- Keys are canonical (each bucket sorted + deduped), so equality is a reliable
-- collision test, and 'renderBrainKey' \/ 'parseBrainKey' give a stable textual
-- identity for storage.
module LLM.Brain.Key
  ( BrainKey (..)
  , emptyKey
  , nullKey
  , canonicalKey
  , keyTerms
  , keyOf
  , canonicalizeTerms
  , renderBrainKey
  , parseBrainKey
  ) where

import Data.Char (isAlphaNum)
import Data.List (nub, sort)
import qualified Data.Text as T

import Effectful (Eff, (:>))

import LLM.Brain.Lexicon (Lexicon, PartOfSpeech (..), classify, lemma)

-- | The noun \/ verb \/ adjective lemma sets of some text. Always held in
-- canonical form (see 'canonicalKey'): each list sorted, deduped, no blanks.
data BrainKey = BrainKey
  { bkNouns :: [T.Text]
  , bkVerbs :: [T.Text]
  , bkAdjs  :: [T.Text]
  } deriving (Eq, Ord, Show)

-- | The key of text with no indexable content.
emptyKey :: BrainKey
emptyKey = BrainKey [] [] []

-- | Does this key carry no terms at all? (Such an entry can never be recalled
-- by overlap and is the cataloguer's signal that text was pure noise.)
nullKey :: BrainKey -> Bool
nullKey k = null (bkNouns k) && null (bkVerbs k) && null (bkAdjs k)

-- | Canonicalise three raw bucket lists into a 'BrainKey': drop blanks, dedupe,
-- sort. The single chokepoint every key passes through, so equal keys are
-- genuinely the same set regardless of construction order.
canonicalKey :: [T.Text] -> [T.Text] -> [T.Text] -> BrainKey
canonicalKey ns vs as = BrainKey (canon ns) (canon vs) (canon as)
  where
    canon = sort . nub . filter (not . T.null)

-- | Every distinct term in the key (across all buckets) — the entries a term
-- should be findable under in an inverted index.
keyTerms :: BrainKey -> [T.Text]
keyTerms k = nub (bkNouns k ++ bkVerbs k ++ bkAdjs k)

-- | READ path: derive the key of a prompt deterministically. Each token is
-- classified and lemmatised via the 'Lexicon'; noise is dropped; a 'Both' word
-- is bucketed as both noun and verb.
keyOf :: (Lexicon :> es) => T.Text -> Eff es BrainKey
keyOf txt = do
  classified <- mapM classifyTok (tokenize txt)
  let ns = [l | (p, l) <- classified, p == Noun || p == Both]
      vs = [l | (p, l) <- classified, p == Verb || p == Both]
      as = [l | (p, l) <- classified, p == Adjective]
  pure (canonicalKey ns vs as)
  where
    classifyTok w = do
      p <- classify w
      l <- lemma w
      pure (p, l)

-- | WRITE path: turn the cataloguer's chosen (noun, verb, adjective) term lists
-- into a canonical key, lemmatising each term through the same 'Lexicon' the
-- read path uses.
canonicalizeTerms
  :: (Lexicon :> es)
  => [T.Text] -> [T.Text] -> [T.Text] -> Eff es BrainKey
canonicalizeTerms ns vs as = do
  ns' <- mapM lemma ns
  vs' <- mapM lemma vs
  as' <- mapM lemma as
  pure (canonicalKey ns' vs' as')

-- | Split text into alphanumeric tokens (the 'Lexicon' lowercases + strips
-- non-letters per token, so we only need to break on separators here).
tokenize :: T.Text -> [T.Text]
tokenize = filter (not . T.null) . T.split (not . isAlphaNum)

-- | A stable textual form, e.g. @"n:bark,dog|v:run|a:happy"@. Terms are
-- lemmatised alphabetic words, so they never contain the @,@ or @|@ separators.
renderBrainKey :: BrainKey -> T.Text
renderBrainKey k =
  T.intercalate "|"
    [ "n:" <> T.intercalate "," (bkNouns k)
    , "v:" <> T.intercalate "," (bkVerbs k)
    , "a:" <> T.intercalate "," (bkAdjs k)
    ]

-- | Inverse of 'renderBrainKey'. Returns 'Nothing' on malformed input.
parseBrainKey :: T.Text -> Maybe BrainKey
parseBrainKey t = case T.splitOn "|" t of
  [n, v, a] -> BrainKey <$> stripList "n:" n <*> stripList "v:" v <*> stripList "a:" a
  _         -> Nothing
  where
    stripList pfx s = case T.stripPrefix pfx s of
      Nothing   -> Nothing
      Just rest -> Just (filter (not . T.null) (T.splitOn "," rest))
