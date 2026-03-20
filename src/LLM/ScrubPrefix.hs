-- | Aeson field label modifier for stripping record prefixes.
--
-- Used with @deriveJSON@ to remove the underscore-delimited prefix from record
-- fields so the JSON keys match the bare field names. For example, a field
-- @_foo_bar@ with @scrubPrefix \"_foo_\"@ serializes as @\"bar\"@.
module LLM.ScrubPrefix where

import Data.Aeson

-- | Build Aeson 'Options' that strip a prefix from every field label.
--
-- > deriveJSON (scrubPrefix "_cwr_") ''ContentWithRole
-- > -- _cwr_role   -> "role"
-- > -- _cwr_content -> "content"
scrubPrefix :: String -> Options
scrubPrefix s =
  defaultOptions { fieldLabelModifier = drop (length s) }
