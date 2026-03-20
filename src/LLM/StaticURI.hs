{-# LANGUAGE TemplateHaskell #-}

-- | Compile-time URI validation via Template Haskell.
module LLM.StaticURI (staticURI) where

import Language.Haskell.TH (Q, Exp)
import Network.URI (parseURI)

-- | Validate a URI string at compile time via Template Haskell.
-- Produces a compilation error if the string is not a valid URI.
--
-- @
-- myURI = $(staticURI \"https://example.com/api\")
-- @
staticURI :: String -> Q Exp
staticURI s = case parseURI s of
  Nothing -> fail $ "staticURI: invalid URI: " <> s
  Just _  -> [| case parseURI s of
                  Just uri -> uri
                  Nothing  -> error "staticURI: impossible - validated at compile time"
              |]
