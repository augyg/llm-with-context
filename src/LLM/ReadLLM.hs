{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Description: Parsec-based typeclass for extracting typed values from LLM output.
-- Copyright: (c) lazyLambda, 2024-2026
-- License: MIT
-- Maintainer: galen.sprout@gmail.com
--
-- MERGE / cross-stream-type compat: Scrappy.Scrape.scrape's signature is
-- `ScraperT a -> Html -> Maybe [a]` where `ScraperT a = ParsecT Html ()
-- Identity a` and `Scrappy.Types.Html` is a type synonym whose definition
-- *varies by scrappy-core version*: some pin `type Html = String`, others
-- `type Html = Text`. Pinning the readLLM class to one stream type breaks
-- the other version.
--
-- Fix: scrappy-json's `Scrappy.JSON.Record` combinators (jInt / jString /
-- jBool / jArray / field / ...) are themselves Stream-polymorphic
-- (@Stream s m Char => ParsecT s u m a@) as of the upstream rev pinned
-- by shell.nix. We just re-use them. No local wrappers; no duplicated
-- JSON parsing logic lives here — scrappy-json is the single source of
-- truth. `readLLM` stays Stream-polymorphic too, so
-- `search = scrape readLLM` instantiates to whatever stream type the
-- current scrappy-core version uses for `Html`.
module LLM.ReadLLM
  ( ReadLLM(..)
  ) where

import Scrappy.JSON.Record (jInt, jInteger, jDouble, jBool, jNull, jString, jArray)
import Scrappy.Scrape (scrape)
import Scrappy.Types (Html)

import Text.Parsec (ParsecT, Stream)

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
--
-- Stream-polymorphic — scrappy-json's primitives are themselves
-- @Stream s m Char@-polymorphic, so GHC instantiates the stream type at
-- the call site. In particular, `search = scrape readLLM` works
-- regardless of whether Scrappy's `Html` resolves to `String` or `Text`.
class ReadLLM a where
  readLLM :: (Stream s m Char) => ParsecT s u m a
  search  :: Html -> Maybe [a]
  search = scrape readLLM

instance ReadLLM Int where
  readLLM = jInt

instance ReadLLM Integer where
  readLLM = jInteger

instance ReadLLM Double where
  readLLM = jDouble

instance ReadLLM Bool where
  readLLM = jBool

-- The overlapping-pragma is kept so an explicit `String` instance request
-- picks jString (the string-body parser) rather than the list-of-char
-- interpretation that would come from the `[a]` instance.
instance {-# OVERLAPPING #-} ReadLLM String where
  readLLM = jString

instance ReadLLM () where
  readLLM = jNull

instance ReadLLM a => ReadLLM [a] where
  readLLM = jArray readLLM
