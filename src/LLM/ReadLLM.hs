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
-- Fix: don't force a stream type at all. `readLLM` is now polymorphic over
-- `Stream s m Char`, so GHC instantiates it at whatever stream `scrape`
-- expects. The underlying JSON primitives are already Stream-polymorphic in
-- `Scrappy.JSON.Primitives`; the thin wrappers in `Scrappy.JSON.Record`
-- happen to specialise them to `String`, so we re-wrap the primitives
-- directly here to stay polymorphic. `search = scrape readLLM` now
-- compiles against any scrappy-core version.
module LLM.ReadLLM
  ( ReadLLM(..)
    -- * Stream-polymorphic JSON primitives
    --
    -- Use these when writing ReadLLM instances instead of
    -- Scrappy.JSON.Record's jInt/jString/... — the latter specialise the
    -- parser to the String stream and are incompatible with the
    -- Stream-polymorphic 'readLLM' class signature.
  , jStringP
  , jIntP
  , jIntegerP
  , jDoubleP
  , jBoolP
  , jNullP
  , jArrayP
  ) where

-- Build the ReadLLM primitives directly on top of Scrappy.JSON.Primitives,
-- which are already Stream-polymorphic. The Record-module wrappers (jInt /
-- jString / ...) are only aliases that happen to specialise to String; by
-- staying polymorphic here we avoid that pinning.
import Scrappy.JSON.Primitives (jsonStringBody, jsonNumber)
import Scrappy.Scrape (scrape)
import Scrappy.Types (Html)

import Text.Parsec
  ( ParsecT, Stream
  , char, oneOf, parserZero, sepBy, skipMany, string, try, (<|>)
  )

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
-- | Typeclass for types that can be parsed from LLM output using Parsec.
-- Stream-polymorphic — instances compose the Stream-polymorphic primitives
-- below (which in turn wrap Scrappy.JSON.Primitives), so GHC instantiates
-- the stream type at the call site. In particular, `search = scrape readLLM`
-- works regardless of whether Scrappy's `Html` resolves to `String` or `Text`.
class ReadLLM a where
  readLLM :: (Stream s m Char) => ParsecT s u m a
  search  :: Html -> Maybe [a]
  search = scrape readLLM

instance ReadLLM Int where
  readLLM = jIntP

instance ReadLLM Integer where
  readLLM = jIntegerP

instance ReadLLM Double where
  readLLM = jDoubleP

instance ReadLLM Bool where
  readLLM = jBoolP

-- The overlapping-pragma is kept so an explicit `String` instance request
-- picks jStringP (the string-body parser) rather than the list-of-char
-- interpretation that would come from the `[a]` instance.
instance {-# OVERLAPPING #-} ReadLLM String where
  readLLM = jStringP

instance ReadLLM () where
  readLLM = jNullP

instance ReadLLM a => ReadLLM [a] where
  readLLM = jArrayP readLLM

-- ============================================================
-- Stream-polymorphic JSON primitives
--
-- These mirror Scrappy.JSON.Record's jInt / jString / ... but keep the
-- Stream polymorphism of the underlying Scrappy.JSON.Primitives parsers.
-- Record's wrappers specialise everything to `Parsec String u X`, which
-- is incompatible with Scrappy.Scrape.scrape on scrappy-core versions
-- where `Html = Text` — see the module-header note.
-- ============================================================

-- | Whitespace between JSON tokens (Stream-polymorphic).
jsonWhitespaceP :: (Stream s m Char) => ParsecT s u m ()
jsonWhitespaceP = skipMany (oneOf " \t\n\r")

-- | Parse a JSON string body (unescaped). Mirrors @jString@.
jStringP :: (Stream s m Char) => ParsecT s u m String
jStringP = jsonStringBody

-- | Parse a JSON integer. Mirrors @jInt@.
jIntP :: (Stream s m Char) => ParsecT s u m Int
jIntP = do
  s <- jsonNumber
  case reads s of
    [(n, "")] -> pure n
    _         -> parserZero

-- | Parse a JSON Integer. Mirrors @jInteger@.
jIntegerP :: (Stream s m Char) => ParsecT s u m Integer
jIntegerP = do
  s <- jsonNumber
  case reads s of
    [(n, "")] -> pure n
    _         -> parserZero

-- | Parse a JSON number as Double. Mirrors @jDouble@.
jDoubleP :: (Stream s m Char) => ParsecT s u m Double
jDoubleP = do
  s <- jsonNumber
  case reads s of
    [(n, "")] -> pure n
    _         -> parserZero

-- | Parse a JSON boolean. Mirrors @jBool@.
jBoolP :: (Stream s m Char) => ParsecT s u m Bool
jBoolP = (try (string "true") >> pure True)
     <|> (string "false" >> pure False)

-- | Parse a JSON null. Mirrors @jNull@.
jNullP :: (Stream s m Char) => ParsecT s u m ()
jNullP = string "null" >> pure ()

-- | Parse a JSON array, applying the element parser to each element.
-- Mirrors @jArray@.
jArrayP :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m [a]
jArrayP elemP = do
  _ <- char '['
  jsonWhitespaceP
  elems <- (jsonWhitespaceP >> elemP) `sepBy` try (jsonWhitespaceP >> char ',')
  jsonWhitespaceP
  _ <- char ']'
  pure elems
