{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Capability classes — one per LLM intent.
--
-- The @APIProvider@ kind ("LLM.Types") enumerates deployment surfaces; each
-- surface implements an INTENT by providing the corresponding capability
-- instance. Pipeline code names the intent it needs as a constraint, the
-- orchestrator picks the provider, and the type system enforces that
-- routing — calling 'askWithImages' against a provider that lacks vision
-- is a compile error, not a runtime failure.
--
-- The classes deliberately keep their signatures NARROW — text in, text
-- out — and push provider-specific extras (model name, temperature,
-- @--add-dir@ args, auth headers) into the interpreter / config layer.
-- The per-class type families ('ImageInput', 'Schema', 'ToolDef') let
-- each provider model its native input shape without forcing a lowest-
-- common-denominator on the call site.
module LLM.Capability
  ( -- * Transport-failure carrier
    TransportError (..)
    -- * Text-only baseline
  , CanText (..)
    -- * Multimodal
  , CanMultimodal (..)
  , ImageInput
    -- * Structured output
  , CanJsonOutput (..)
    -- * Tool use
  , CanToolUse (..)
  ) where

import Data.Kind (Type)
import qualified Data.Text as T
import Effectful (Eff, (:>))

import LLM.Effect (LLM)
import LLM.Effect.ImageInput (ImageInput)
import LLM.Types (APIProvider)

-- | Transport-level failure surfaced by a capability ask: the
-- interpreter ran but the underlying transport (CLI exit /= 0, HTTP
-- error, decode failure of the wire response) reported a 'Left'. The
-- wrapped 'T.Text' is whatever the interpreter said about the failure
-- — opaque to the capability layer; the consumer is free to log it,
-- bubble it as a domain error, or pattern-match on its substring.
--
-- The classes deliberately surface this via 'Either' instead of
-- 'error': the call site needs the choice of crash-vs-recover, not the
-- library. (See CLAUDE.md "Bubble up errors — never bury them in the
-- wrapper".)
newtype TransportError = TransportError { unTransportError :: T.Text }
  deriving (Show, Eq)

-- | Text-only ask. Every provider implements this — it's the universal
-- baseline. The argument is the prompt body; the result is either a
-- 'TransportError' (interpreter / wire failure) or the response text.
-- Providers that need extra args (model name, temperature, max tokens)
-- handle them via their environment / config, not via this method's
-- signature.
class CanText (p :: APIProvider) where
  askText
    :: (LLM p :> es)
    => T.Text -> Eff es (Either TransportError T.Text)

-- | Multimodal ask. The 'ImageInput' type family maps each provider to
-- its native image-input shape (file paths for the CLI, base64-encoded
-- bytes for HTTP APIs, URLs for some hosted backends). Providers
-- without vision capability simply don't have the instance — calling
-- 'askWithImages' against them is a compile error. The result mirrors
-- 'askText': 'Left TransportError' on interpreter failure, 'Right' on
-- success.
class CanMultimodal (p :: APIProvider) where
  askWithImages
    :: (LLM p :> es)
    => ImageInput p -> T.Text -> Eff es (Either TransportError T.Text)

-- | Structured-output ask. The 'Schema' family is per-provider because
-- how schemas are submitted differs across surfaces (the @claude@ CLI
-- consumes a schema via @--json-schema@; the OpenAI HTTP API uses
-- @response_format@; some providers don't accept a schema at all and
-- rely on prompt-side JSON examples). The result is either a
-- 'TransportError' or the raw response text — the consumer parses it.
class CanJsonOutput (p :: APIProvider) where
  type Schema p :: Type
  askJson
    :: (LLM p :> es)
    => Schema p -> T.Text -> Eff es (Either TransportError T.Text)

-- | Tool-use ask. The 'ToolDef' family is per-provider — each tool-use
-- protocol (Anthropic, OpenAI, Google) has its own serialisation.
class CanToolUse (p :: APIProvider) where
  type ToolDef p :: Type
  askWithTools :: (LLM p :> es) => [ToolDef p] -> T.Text -> Eff es T.Text
