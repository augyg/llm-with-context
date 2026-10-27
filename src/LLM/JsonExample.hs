{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Description: Generate JSON example strings from Haskell types via GHC Generics.
-- Copyright: (c) lazyLambda, 2024-2026
-- License: MIT
-- Maintainer: galen.sprout@gmail.com
module LLM.JsonExample
  ( -- * Typeclass
    JsonExample(..)
  , genericJsonExample
  , jsonResponsePrompt
    -- * Field name transform
  , stripFieldPrefix
    -- * Generic machinery (for extending)
  , GJsonObject(..)
  , GJsonFields(..)
  ) where

import Data.Char (isLower, toLower)
import Data.Kind (Type)
import Data.List (intercalate)
import Data.Proxy (Proxy(..))
import GHC.Generics (Generic, Rep, M1(..), K1(..), R, S, C, D, Selector, selName, (:*:)(..), (:+:)(..))

-- | Typeclass for generating JSON example strings from types.
-- Uses GHC Generics by default — derive Generic and write an empty instance:
--
-- @
-- data MyResponse = MyResponse { mrField :: Int } deriving (Generic)
-- instance JsonExample MyResponse
-- -- generates: {"field": N}
-- @
class JsonExample a where
  jsonExample :: Proxy a -> String
  default jsonExample :: (Generic a, GJsonObject (Rep a)) => Proxy a -> String
  jsonExample = genericJsonExample

-- | Generate a JSON example from a Generic type representation.
genericJsonExample :: forall a. (Generic a, GJsonObject (Rep a)) => Proxy a -> String
genericJsonExample _ = gJsonObject (Proxy :: Proxy (Rep a))

-- | Build a standard LLM response prompt: "Respond with ONLY a JSON object:\n" ++ example
jsonResponsePrompt :: JsonExample a => Proxy a -> String
jsonResponsePrompt p = "Respond with ONLY a JSON object:\n" ++ jsonExample p

-- | Strip Haskell field prefix: @"ciTitle"@ -> @"title"@, @"csHook"@ -> @"hook"@
stripFieldPrefix :: String -> String
stripFieldPrefix s = case dropWhile isLower s of
  []     -> s
  (c:cs) -> toLower c : cs

-- ============================================================
-- Generic: object level (datatype / constructor / sum)
-- ============================================================

class GJsonObject (f :: Type -> Type) where
  gJsonObject :: Proxy f -> String

instance GJsonObject f => GJsonObject (M1 D d f) where
  gJsonObject _ = gJsonObject (Proxy :: Proxy f)

instance GJsonFields f => GJsonObject (M1 C c f) where
  gJsonObject _ = "{" ++ intercalate ", " [show k ++ ": " ++ v | (k, v) <- gJsonFields (Proxy :: Proxy f)] ++ "}"

instance (GJsonObject a, GJsonObject b) => GJsonObject (a :+: b) where
  gJsonObject _ = gJsonObject (Proxy :: Proxy a) ++ " | " ++ gJsonObject (Proxy :: Proxy b)

-- ============================================================
-- Generic: field level (products / selectors)
-- ============================================================

class GJsonFields (f :: Type -> Type) where
  gJsonFields :: Proxy f -> [(String, String)]

instance (GJsonFields a, GJsonFields b) => GJsonFields (a :*: b) where
  gJsonFields _ = gJsonFields (Proxy :: Proxy a) ++ gJsonFields (Proxy :: Proxy b)

instance (Selector s, JsonExample a) => GJsonFields (M1 S s (K1 R a)) where
  gJsonFields _ = [(stripFieldPrefix (selName (undefined :: M1 S s (K1 R a) p)),
                     jsonExample (Proxy :: Proxy a))]

-- ============================================================
-- Leaf instances
-- ============================================================

instance JsonExample Int where jsonExample _ = "N"
instance JsonExample Double where jsonExample _ = "N.N"
instance {-# OVERLAPPING #-} JsonExample String where jsonExample _ = "\"...\""
instance {-# OVERLAPPABLE #-} JsonExample a => JsonExample [a] where
  jsonExample _ = "[" ++ jsonExample (Proxy :: Proxy a) ++ ", ...]"
instance JsonExample a => JsonExample (Maybe a) where
  jsonExample _ = jsonExample (Proxy :: Proxy a) ++ " | null"
instance (JsonExample a, JsonExample b) => JsonExample (Either a b) where
  jsonExample _ = jsonExample (Proxy :: Proxy a) ++ " | " ++ jsonExample (Proxy :: Proxy b)
