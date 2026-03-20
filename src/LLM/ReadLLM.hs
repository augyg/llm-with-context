{-# LANGUAGE FlexibleInstances #-}

module LLM.ReadLLM
  ( ReadLLM(..)
  ) where

import Scrappy.JSON.Record (jString, jInt, jInteger, jDouble, jBool, jNull, jArray)
import Scrappy.Scrape (scrape)

import Text.Parsec (Parsec)

-- | Typeclass for types that can be parsed from LLM output using Parsec.
-- Replaces the 'Read' constraint for LLM response parsing.
--
-- Write an instance by composing scrappy-json combinators:
--
-- @
-- instance ReadLLM MovieReview where
--   readLLM = do
--     _ <- char '{'
--     t <- field "title"   jString
--     r <- field "rating"  jInt
--     s <- field "summary" jString
--     pure $ MovieReview t r s
-- @
--
-- Then use 'search' to extract from noisy LLM output:
--
-- @
-- search "{\"title\": \"The Matrix\", ...}" :: Maybe [MovieReview]
-- @
class ReadLLM a where
  readLLM :: Parsec String () a
  search  :: String -> Maybe [a]
  search = scrape readLLM

instance ReadLLM Int where
  readLLM = jInt

instance ReadLLM Integer where
  readLLM = jInteger

instance ReadLLM Double where
  readLLM = jDouble

instance ReadLLM Bool where
  readLLM = jBool

instance {-# OVERLAPPING #-} ReadLLM String where
  readLLM = jString

instance ReadLLM () where
  readLLM = jNull

instance ReadLLM a => ReadLLM [a] where
  readLLM = jArray readLLM
