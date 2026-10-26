{-# LANGUAGE OverloadedStrings #-}
module LLM.Tools.BashEdit
  ( bashTool
  , editTool
  , execBash
  , execEdit
  , defaultToolExecutor
  ) where

import LLM.Types

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import Data.Aeson.Key (toText)
import qualified Data.Text.IO as T
import System.Exit (ExitCode(..))
import System.Process (readCreateProcessWithExitCode, shell)
import System.Directory (doesFileExist)

-- | Bash tool definition.
bashTool :: ToolDef
bashTool = ToolDef
  { _toolDef_name        = "bash"
  , _toolDef_description = "Run a bash shell command. Use this for file listing, searching, building, running tests, etc."
  , _toolDef_inputSchema = Data.Aeson.object
      [ "type" .= ("object" :: T.Text)
      , "properties" .= Data.Aeson.object
          [ "command" .= Data.Aeson.object
              [ "type"        .= ("string" :: T.Text)
              , "description" .= ("The bash command to execute" :: T.Text)
              ]
          ]
      , "required" .= [ "command" :: T.Text ]
      ]
  }

-- | Edit tool definition (Claude-style).
editTool :: ToolDef
editTool = ToolDef
  { _toolDef_name        = "edit"
  , _toolDef_description = "Edit a file by replacing an exact string with new text. The old_string must match exactly."
  , _toolDef_inputSchema = Data.Aeson.object
      [ "type" .= ("object" :: T.Text)
      , "properties" .= Data.Aeson.object
          [ "path"       .= Data.Aeson.object [ "type" .= ("string" :: T.Text), "description" .= ("Absolute or relative file path" :: T.Text) ]
          , "old_string" .= Data.Aeson.object [ "type" .= ("string" :: T.Text), "description" .= ("Exact text to find and replace" :: T.Text) ]
          , "new_string" .= Data.Aeson.object [ "type" .= ("string" :: T.Text), "description" .= ("Replacement text" :: T.Text) ]
          ]
      , "required" .= [ "path" :: T.Text, "old_string" :: T.Text, "new_string" :: T.Text ]
      ]
  }

-- | Execute a single tool use and return a ToolResult.
defaultToolExecutor :: [ToolUse] -> IO [ToolResult]
defaultToolExecutor = mapM $ \tu -> do
  content <- case _toolUse_name tu of
    "bash" -> execBash (_toolUse_input tu)
    "edit" -> execEdit (_toolUse_input tu)
    other  -> pure $ "Unknown tool: " <> other
  pure $ ToolResult (_toolUse_id tu) content

execBash :: Value -> IO T.Text
execBash v = case fromJSON v of
  Error e -> pure $ T.pack $ "Invalid bash arguments: " <> e
  Success (Data.Aeson.Object o) -> case KM.lookup "command" o of
    Just (String cmd) -> do
      (exitCode, out, err) <- readCreateProcessWithExitCode (shell $ T.unpack cmd) ""
      let output = T.pack $ out <> (if null err then "" else "\nstderr: " <> err)
      pure $ case exitCode of
        ExitSuccess   -> output
        ExitFailure c -> output <> "\nExit code: " <> T.pack (show c)
    _ -> pure "Missing or non-string 'command' in bash arguments"
  Success _ -> pure "Expected Data.Aeson.object for bash arguments"

execEdit :: Value -> IO T.Text
execEdit v = case fromJSON v of
  Error e -> pure $ T.pack $ "Invalid edit arguments: " <> e
  Success (Data.Aeson.Object o) -> do
    let get k = case KM.lookup k o of
          Just (String t) -> Right t
          _               -> Left $ "Missing or non-string field: " <> toText k
    case (,,) <$> get "path" <*> get "old_string" <*> get "new_string" of
      Left err -> pure err
      Right (path, oldStr, newStr) -> do
        exists <- doesFileExist (T.unpack path)
        if not exists
          then pure $ "File not found: " <> path
          else do
            content <- T.readFile (T.unpack path)
            if not (oldStr `T.isInfixOf` content)
              then pure $ "old_string not found in " <> path
              else do
                let newContent = T.replace oldStr newStr content
                T.writeFile (T.unpack path) newContent
                pure $ "Edited " <> path
  Success _ -> pure "Expected Data.Aeson.object for edit arguments"
