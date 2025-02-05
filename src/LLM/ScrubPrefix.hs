module LLM.ScrubPrefix where

import Data.Aeson

scrubPrefix :: String -> Options
scrubPrefix s =
  defaultOptions { fieldLabelModifier = drop (length s) }
