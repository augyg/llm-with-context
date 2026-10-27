{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | The bridge between a model's tool calls and our typed 'Tool' effect. A
-- 'ToolUse' from the model maps DIRECTLY onto the 'Tool' GADT operations
-- ('run' / 'readFile' / …) — no generic string-keyed registry indirection. The
-- 'Tool' effect IS the tool surface the model sees: 'toolDefs' advertises those
-- operations to the model, and 'dispatchToolUse' decodes a call and invokes the
-- matching GADT op.
module LLM.Tooling
  ( -- * The model ⇄ Tool-effect bridge
    toolDefs
  , dispatchToolUse
  , renderToolResult
    -- * Read-only tool-set env (set once at setup; askTooled reads it)
  , ToolSet (..)
  , defaultToolSet
  , runToolSet
    -- * Tooled ask: one final response, intermediate tool round-trips hidden
  , askTooled
  , askTooledWith
  , askTooledAnthropic
  , askTooledOpenAI
  , defaultMaxToolIterations
    -- * One-round primitive (caller drives the loop)
  , stepTooled
  , appendToolRound
  ) where

import Prelude hiding (readFile, writeFile)

import Control.Monad (forM)
import Data.Aeson (FromJSON (..), Result (..), Value, fromJSON, object, withObject, (.:), (.:?), (.=))
import qualified Data.Aeson.Key as Key
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Effectful (Eff, (:>))
import Effectful.Reader.Static (Reader, asks, runReader)

import LLM.Effect (LLM, askTools)
import LLM.Effect.Tool
  ( ShellCommand (..)
  , Tool
  , ToolResult (..)
  , fileExists
  , listDirectory
  , readFile
  , runCommand
  , writeFile
  )
import LLM.Types
  ( APIProvider (..)
  , Block (..)
  , ContentWithRole (..)
  , GPTRole (..)
  , RichMessage (..)
  , ToolDef (..)
  , ToolTurn (..)
  , ToolUse (..)
  )

-- | The 'Tool' effect's operations, advertised to the model as callable tools.
toolDefs :: [ToolDef]
toolDefs =
  [ ToolDef "grep" "Search for a pattern in a file or directory."
      (objectSchema [("pattern", stringSchema), ("path", stringSchema), ("recursive", boolSchema), ("ignore_case", boolSchema)] ["pattern", "path"])
  , ToolDef "ls" "List a directory."
      (objectSchema [("path", stringSchema), ("all", boolSchema), ("long", boolSchema)] ["path"])
  , ToolDef "find" "Find files under a root matching a name pattern."
      (objectSchema [("root", stringSchema), ("name", stringSchema)] ["root", "name"])
  , ToolDef "read_file" "Read a file's contents."
      (objectSchema [("path", stringSchema)] ["path"])
  , ToolDef "write_file" "Write text to a file (overwrites)."
      (objectSchema [("path", stringSchema), ("contents", stringSchema)] ["path", "contents"])
  , ToolDef "list_directory" "List the entries of a directory."
      (objectSchema [("path", stringSchema)] ["path"])
  , ToolDef "file_exists" "Check whether a file exists."
      (objectSchema [("path", stringSchema)] ["path"])
  ]

-- | Map a model 'ToolUse' directly onto the 'Tool' GADT: decode the JSON
-- arguments, invoke the matching operation, render the result to text. An
-- unknown name or bad arguments come back as an @error: …@ string (so the model
-- receives a normal tool result, not a crash).
dispatchToolUse :: forall es. (Tool :> es) => ToolUse -> Eff es T.Text
dispatchToolUse (ToolUse _callId name input) = case name of
  "grep" -> withArgs $ \(GrepInput pat path rec ic) ->
    renderToolResult <$> runCommand (Grep pat (T.unpack path) (fromMaybe False rec) (fromMaybe False ic))
  "ls" -> withArgs $ \(LsInput path showAll long) ->
    renderToolResult <$> runCommand (Ls (T.unpack path) (fromMaybe False showAll) (fromMaybe False long))
  "find" -> withArgs $ \(FindInput root namePat) ->
    renderToolResult <$> runCommand (Find (T.unpack root) namePat)
  "read_file" -> withArgs $ \(PathInput p) -> eitherText id <$> readFile (T.unpack p)
  "write_file" -> withArgs $ \(WriteInput p c) -> eitherText (const "ok") <$> writeFile (T.unpack p) c
  "list_directory" -> withArgs $ \(PathInput p) -> eitherText (T.intercalate "\n" . map T.pack) <$> listDirectory (T.unpack p)
  "file_exists" -> withArgs $ \(PathInput p) -> boolText <$> fileExists (T.unpack p)
  other -> pure ("error: unknown tool '" <> other <> "'")
  where
    withArgs :: FromJSON a => (a -> Eff es T.Text) -> Eff es T.Text
    withArgs k = case fromJSON input of
      Success a -> k a
      Error e -> pure ("error: bad arguments for '" <> name <> "': " <> T.pack e)
    eitherText :: (a -> T.Text) -> Either T.Text a -> T.Text
    eitherText ok = either ("error: " <>) ok
    boolText b = if b then "true" else "false"

-- | Render a 'ToolResult' to the text handed back to the model.
renderToolResult :: ToolResult -> T.Text
renderToolResult (ToolResult code out err) =
  T.intercalate "\n"
    ["exit_code: " <> T.pack (show code), "stdout:", out, "stderr:", err]

-- JSON-Schema fragments ------------------------------------------------------

stringSchema :: Value
stringSchema = object ["type" .= ("string" :: T.Text)]

boolSchema :: Value
boolSchema = object ["type" .= ("boolean" :: T.Text)]

objectSchema :: [(T.Text, Value)] -> [T.Text] -> Value
objectSchema props required =
  object
    [ "type" .= ("object" :: T.Text)
    , "properties" .= object [Key.fromText k .= v | (k, v) <- props]
    , "required" .= required
    ]

-- Typed inputs decoded from the model's JSON arguments -----------------------

data GrepInput = GrepInput T.Text T.Text (Maybe Bool) (Maybe Bool)
instance FromJSON GrepInput where
  parseJSON = withObject "GrepInput" $ \o ->
    GrepInput <$> o .: "pattern" <*> o .: "path" <*> o .:? "recursive" <*> o .:? "ignore_case"

data LsInput = LsInput T.Text (Maybe Bool) (Maybe Bool)
instance FromJSON LsInput where
  parseJSON = withObject "LsInput" $ \o ->
    LsInput <$> o .: "path" <*> o .:? "all" <*> o .:? "long"

data FindInput = FindInput T.Text T.Text
instance FromJSON FindInput where
  parseJSON = withObject "FindInput" $ \o -> FindInput <$> o .: "root" <*> o .: "name"

newtype PathInput = PathInput T.Text
instance FromJSON PathInput where
  parseJSON = withObject "PathInput" $ \o -> PathInput <$> o .: "path"

data WriteInput = WriteInput T.Text T.Text
instance FromJSON WriteInput where
  parseJSON = withObject "WriteInput" $ \o -> WriteInput <$> o .: "path" <*> o .: "contents"

-- Tooled ask -----------------------------------------------------------------

-- | Safety bound so a misbehaving model can't loop forever.
defaultMaxToolIterations :: Int
defaultMaxToolIterations = 16

-- | The advertised tool set + loop cap, established ONCE at setup and held
-- read-only (via 'Reader'). 'askTooled' reads it rather than taking it per call
-- — consistent with the "globally-decided → Reader" rule.
data ToolSet = ToolSet
  { _toolSet_defs :: [ToolDef]
  , _toolSet_maxIterations :: Int
  }

-- | Default tool set: the 'Tool' effect's operations ('toolDefs') + the default
-- iteration cap.
defaultToolSet :: ToolSet
defaultToolSet = ToolSet toolDefs defaultMaxToolIterations

-- | Establish the read-only tool set for a computation (the "set once at setup"
-- step). Just 'runReader' specialised to 'ToolSet'.
runToolSet :: ToolSet -> Eff (Reader ToolSet : es) a -> Eff es a
runToolSet = runReader

-- | Ask provider @p@ with the 'Tool' effect exposed as its tools, returning the
-- ONE final response. The intermediate tool round-trips are handled internally:
-- each model tool call is dispatched onto the 'Tool' GADT ('dispatchToolUse'),
-- its result fed back, repeat until the model answers with no further tool
-- calls. The advertised tools + loop cap come from the read-only 'ToolSet' env
-- (set once via 'runToolSet'). Pin the provider: @askTooled \@'AnthropicHttp [...]@.
askTooled
  :: forall p es. (LLM p :> es, Tool :> es, Reader ToolSet :> es)
  => [ContentWithRole] -> Eff es (Either T.Text T.Text)
askTooled contents = do
  defs <- asks _toolSet_defs
  maxIters <- asks _toolSet_maxIterations
  askTooledWith @p defs maxIters contents

-- | 'askTooled' with an explicit tool-definition set and iteration cap. Defined
-- as a loop over 'stepTooled' (the one-round primitive) so the looped and
-- caller-driven paths share one definition.
askTooledWith
  :: forall p es. (LLM p :> es, Tool :> es)
  => [ToolDef] -> Int -> [ContentWithRole] -> Eff es (Either T.Text T.Text)
askTooledWith defs maxIters initial = loop initialMessages 0
  where
    initialMessages = [RichMessage User (map (BlockText . _cwr_content) initial)]
    loop :: [RichMessage] -> Int -> Eff es (Either T.Text T.Text)
    loop msgs n
      | n >= maxIters = pure (Left "askTooled: max iterations reached")
      | otherwise = stepTooled @p defs msgs >>= \case
          Left err -> pure (Left err)
          Right (t, resultBlocks)
            | null (_toolTurn_toolUses t) -> pure (Right (fromMaybe "" (_toolTurn_text t)))
            | otherwise -> loop (appendToolRound msgs t resultBlocks) (n + 1)

-- | One round of the tool loop, returning control to the caller. Asks provider
-- @p@ once with the tool defs + messages, dispatches whatever tool calls the
-- model made ('dispatchToolUse'), and hands back BOTH the raw 'ToolTurn' (what
-- the model said / wanted) and the 'BlockToolResult's from running those calls.
-- The caller assembles the next message list (see 'appendToolRound') and decides
-- whether to loop. Pin the provider: @stepTooled \@'AnthropicHttp defs msgs@.
stepTooled
  :: forall p es. (LLM p :> es, Tool :> es)
  => [ToolDef] -> [RichMessage] -> Eff es (Either T.Text (ToolTurn, [Block]))
stepTooled defs msgs = do
  turn <- askTools @p defs msgs
  case turn of
    Left err -> pure (Left err)
    Right t -> do
      resultBlocks <- forM (_toolTurn_toolUses t) $ \u -> do
        result <- dispatchToolUse u
        pure (BlockToolResult (_toolUse_id u) result False)
      pure (Right (t, resultBlocks))

-- | Append one tool round to a message list: the assistant's blocks, then a
-- user turn carrying the tool-result blocks. Use after 'stepTooled' when
-- looping manually.
appendToolRound :: [RichMessage] -> ToolTurn -> [Block] -> [RichMessage]
appendToolRound msgs t resultBlocks =
  msgs ++ [RichMessage Assistant (_toolTurn_assistantBlocks t), RichMessage User resultBlocks]

-- | 'askTooled' pinned to Anthropic.
askTooledAnthropic
  :: (LLM 'AnthropicHttp :> es, Tool :> es, Reader ToolSet :> es)
  => [ContentWithRole] -> Eff es (Either T.Text T.Text)
askTooledAnthropic = askTooled @'AnthropicHttp

-- | 'askTooled' pinned to OpenAI.
askTooledOpenAI
  :: (LLM 'OpenAIHttp :> es, Tool :> es, Reader ToolSet :> es)
  => [ContentWithRole] -> Eff es (Either T.Text T.Text)
askTooledOpenAI = askTooled @'OpenAIHttp
