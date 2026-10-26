{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Description: Server-Sent-Events parser for Anthropic's streaming
-- @\/v1\/messages@ response.
-- Copyright: (c) lazyLambda, 2026
-- License: MIT
-- Maintainer: galen.sprout@gmail.com
--
-- Anthropic streams token deltas as SSE: each event is one or more
-- @event: <name>@ lines followed by one @data: <json>@ line and a
-- blank line terminator. The event types we care about are
-- @content_block_delta@ (carries @{type:"text_delta",text:"..."}@)
-- and @message_stop@ (end-of-turn). This module provides the pure
-- parser; the HTTP driver lives in "LLM.Provider.AnthropicOAuth.Ask".
--
-- Protocol reference: Anthropic Streaming Messages docs
-- (https://docs.anthropic.com/en/api/messages-streaming). Event
-- names + payload shapes verified against the published spec on
-- 2026-10-25.
module LLM.Provider.AnthropicOAuth.Stream
  ( -- * Event sum
    StreamEvent (..)
    -- * Incremental parser
  , StreamBuffer
  , emptyStreamBuffer
  , feedBytes
  ) where

import Data.Aeson (FromJSON (..), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)

-- | One parsed SSE event from the streaming response. The @Unknown@
-- constructor is intentional: Anthropic may add new event types
-- (@ping@, future @tool_use_delta@, etc.) and the parser should
-- surface them without crashing so the caller can log or ignore.
-- Fatal failures use 'StreamParseError' with a specific reason.
data StreamEvent
  = EventMessageStart
    -- ^ @message_start@: the first event, carries message metadata
    -- (model, usage). We don't inspect it; just acknowledge receipt.
  | EventContentBlockStart Int
    -- ^ @content_block_start@: a new content block begins (text,
    -- tool_use, etc.). The 'Int' is the block index.
  | EventContentBlockDelta Int Text
    -- ^ @content_block_delta@ with a @text_delta@ payload. The
    -- 'Text' is the fresh token(s); the caller concatenates these in
    -- order to reconstruct the assistant turn. Non-text deltas
    -- (currently @input_json_delta@ for tool_use blocks) are
    -- surfaced as 'EventUnknown' because Phase B.5 handles them.
  | EventContentBlockStop Int
    -- ^ @content_block_stop@: the current block is finalised. Useful
    -- for driving UI boundaries (e.g. inserting a newline).
  | EventMessageDelta
    -- ^ @message_delta@: carries stop_reason / usage updates we
    -- don't currently expose. Opaque for now.
  | EventMessageStop
    -- ^ @message_stop@: the server signals end-of-turn. The HTTP
    -- connection may still deliver trailing whitespace; parsers
    -- should treat this as the authoritative "turn ended" marker.
  | EventPing
    -- ^ @ping@: Anthropic sends these periodically to keep the
    -- connection alive. No payload of interest.
  | EventError Text
    -- ^ @error@ event: server-reported error inside the stream
    -- (distinct from a 4xx/5xx on the initial response). The 'Text'
    -- is the server's rendering.
  | EventUnknown Text
    -- ^ Event name we don't recognise. Payload intentionally
    -- discarded (if we needed it we'd add a constructor); the
    -- caller can log the name for forward-compat visibility.
  | StreamParseError Text
    -- ^ A frame failed to parse (missing @data:@ line, non-JSON
    -- payload, etc.). The 'Text' names the issue. Hard failure —
    -- the stream state is undefined past this point; callers should
    -- abort the request.
  deriving (Show, Eq, Generic)

-- | Buffer carrying bytes that haven't yet formed a complete @\n\n@-
-- terminated SSE frame. Opaque; 'feedBytes' threads it through
-- successive HTTP read-chunks.
newtype StreamBuffer = StreamBuffer { unStreamBuffer :: ByteString }
  deriving (Show, Eq)

-- | Starting state. Pass to the first 'feedBytes' call.
emptyStreamBuffer :: StreamBuffer
emptyStreamBuffer = StreamBuffer BS.empty

-- | Feed one HTTP read-chunk into the buffer, drain all complete
-- frames into 'StreamEvent's, return the (possibly partial) tail
-- buffer. Pure — the caller does the IO.
--
-- Boundary rule: SSE frames are separated by @\\n\\n@ (two LF
-- bytes) per the spec. Partial frame at the tail is retained in the
-- returned 'StreamBuffer'. 'feedBytes emptyStreamBuffer ""' returns
-- @(emptyStreamBuffer, [])@.
feedBytes :: StreamBuffer -> ByteString -> (StreamBuffer, [StreamEvent])
feedBytes (StreamBuffer prior) fresh =
  let combined = prior <> fresh
      (completeFrames, tail') = splitFrames combined
      events = map parseFrame completeFrames
  in (StreamBuffer tail', events)

-- | Split a byte buffer on @\\n\\n@ delimiters. Returns the list of
-- complete frames (without the delimiter) and the tail that lacks a
-- terminator.
splitFrames :: ByteString -> ([ByteString], ByteString)
splitFrames = go []
  where
    delim = BS.pack [0x0A, 0x0A]   -- "\n\n"
    go acc bs = case BS.breakSubstring delim bs of
      (frame, rest)
        | BS.null rest -> (reverse acc, frame)
          -- ^ no delimiter found — everything is tail
        | otherwise ->
            let rest' = BS.drop (BS.length delim) rest
            in go (frame : acc) rest'

-- | Parse one complete frame into a 'StreamEvent'. The frame is a
-- block of @key: value@ lines; we only care about @event:@ (name)
-- and @data:@ (JSON payload). Comment lines (@: ...@) and other
-- SSE-defined fields (@id:@, @retry:@) are ignored.
parseFrame :: ByteString -> StreamEvent
parseFrame frame =
  let ls = BS.split 0x0A frame
      kvs = [(k, BS.drop 2 v) | ln <- ls, Just (k, v) <- [breakField ln]]
      eventName = lookup "event" kvs
      dataField = lookup "data" kvs
  in case (eventName, dataField) of
       (Just name, mdata) -> decodeEvent (TE.decodeUtf8 name) mdata
       (Nothing, _)       -> StreamParseError
         ("SSE frame missing 'event:' line: " <> TE.decodeUtf8 (BS.take 60 frame))
  where
    -- Split one @k: v@ line into (k, ": v"-prefixed rest). We use
    -- BS.drop 2 above to skip the ": ". Rejects lines without ": ".
    breakField :: ByteString -> Maybe (ByteString, ByteString)
    breakField ln =
      let (k, rest) = BS.breakSubstring (BS.pack [0x3A, 0x20]) ln   -- ": "
      in if BS.null rest || BS.null k
         then Nothing
         else Just (k, rest)

-- | Classify a parsed @event:@ name + optional @data:@ payload.
decodeEvent :: Text -> Maybe ByteString -> StreamEvent
decodeEvent name mdata = case name of
  "message_start"        -> EventMessageStart
  "content_block_start"  -> case decodePayload mdata of
    Just (BlockStartPayload i) -> EventContentBlockStart i
    _                          -> EventUnknown name
  "content_block_delta"  -> case decodePayload mdata of
    Just (BlockDeltaPayload i (TextDelta t)) -> EventContentBlockDelta i t
    Just (BlockDeltaPayload _ (NonTextDelta _)) -> EventUnknown name
    _                                        -> EventUnknown name
  "content_block_stop"   -> case decodePayload mdata of
    Just (BlockStopPayload i) -> EventContentBlockStop i
    _                         -> EventUnknown name
  "message_delta"        -> EventMessageDelta
  "message_stop"         -> EventMessageStop
  "ping"                 -> EventPing
  "error"                -> case mdata of
    Just bs -> EventError (TE.decodeUtf8 bs)
    Nothing -> EventError "<no data>"
  other                  -> EventUnknown other
  where
    decodePayload :: FromJSON a => Maybe ByteString -> Maybe a
    decodePayload = (>>= \bs -> case Aeson.eitherDecode (BL.fromStrict bs) of
      Right v -> Just v
      Left _  -> Nothing)

-- Internal payload decoders — only the shapes we actually consume.
-- Unknown fields are tolerated by aeson's default FromJSON behavior.
newtype BlockStartPayload = BlockStartPayload Int
instance FromJSON BlockStartPayload where
  parseJSON = Aeson.withObject "content_block_start" $ \o ->
    BlockStartPayload <$> o .: "index"

newtype BlockStopPayload = BlockStopPayload Int
instance FromJSON BlockStopPayload where
  parseJSON = Aeson.withObject "content_block_stop" $ \o ->
    BlockStopPayload <$> o .: "index"

data BlockDeltaPayload = BlockDeltaPayload Int DeltaBody
instance FromJSON BlockDeltaPayload where
  parseJSON = Aeson.withObject "content_block_delta" $ \o ->
    BlockDeltaPayload <$> o .: "index" <*> o .: "delta"

-- | The @delta@ sub-object. Current shapes:
--
-- * @{type:\"text_delta\",text:\"...\"}@
-- * @{type:\"input_json_delta\",partial_json:\"...\"}@ (tool_use)
--
-- We model the first explicitly and bucket the rest as 'NonTextDelta'
-- so Phase B.5 can extend the sum without this module compiling-off.
data DeltaBody = TextDelta Text | NonTextDelta Text
instance FromJSON DeltaBody where
  parseJSON = Aeson.withObject "delta" $ \o -> do
    ty :: Text <- o .: "type"
    case ty of
      "text_delta" -> TextDelta <$> o .: "text"
      other        -> pure (NonTextDelta other)

