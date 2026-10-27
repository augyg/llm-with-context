{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | The 'ImageInput' open type family — one entry per provider that
-- supports multimodal input. It lives in its own module to break the
-- import cycle that would otherwise form: 'LLM.Effect' carries an
-- 'AskMultimodal' constructor whose payload is @'ImageInput' p@, while
-- 'LLM.Capability' (the home of 'CanMultimodal') needs to import
-- 'LLM.Effect' to constrain its method on @LLM p :> es@. Putting the
-- family here lets both depend on it without depending on each other.
--
-- Each provider's instance is declared next to that provider's runner
-- (e.g. @type instance ImageInput 'AnthropicCli = [FilePath]@ in
-- "LLM.Provider.AnthropicCli"), so the provider owns the shape of the
-- image carrier it accepts.
module LLM.Effect.ImageInput
  ( ImageInput
  ) where

import Data.Kind (Type)

import LLM.Types (APIProvider)

-- | Provider-specific image-input carrier: file paths for the CLI,
-- base64-encoded bytes for HTTP, URLs for hosted backends, etc.
-- Providers without vision capability simply don't have an instance.
type family ImageInput (p :: APIProvider) :: Type
