{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Phase E — Read / Grep / Glob tools mirroring claude-code's
-- canonical shapes. The tool NAMES, descriptions, and input_schema
-- field names are copied verbatim from cli.js so Claude's existing
-- training-time calibration on these specific identifiers carries
-- over directly.
--
-- All three tools accept absolute paths (claude-code's convention).
-- Relative paths work too but are resolved against the harness's
-- CWD, which may not match the user's expectation on long sessions
-- — document in Read's description so the model prefers absolute.
--
-- Executors:
--
--   * 'execRead' — opens the file, applies optional @offset@/@limit@
--     line window, prepends line numbers in cli.js's exact format
--     (@   123\t<content>@), truncates lines over 2000 chars with a
--     "[line truncated: ...]" marker. Returns a diagnostic for
--     missing or empty files.
--   * 'execGrep' — shells out to ripgrep (resolved at compile time
--     via 'staticWhich' per CLAUDE.md). Supports @output_mode@,
--     @-i@, @-n@, @-A@/@-B@/@-C@, @head_limit@, @glob@ filter.
--   * 'execGlob' — uses the @System.FilePath.Glob@ library for
--     pattern matching. Returns matched files sorted by mtime desc
--     (claude-code's behavior — most-recently-modified first helps
--     the model find relevant files faster).
module LLM.Tools.FileOps
  ( readTool
  , grepTool
  , globTool
  , writeTool
  , multiEditTool
  , execRead
  , execGrep
  , execGlob
  , execWrite
  , execMultiEdit
  ) where

import LLM.Types

import Control.Exception (SomeException, try)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Aeson.Key as Key
import Data.Foldable (toList)
import Data.List (sortBy)
import Data.Ord (Down (..), comparing)
import Data.Time.Clock (UTCTime)
import System.Directory (createDirectoryIfMissing, doesFileExist, getModificationTime, renameFile)
import System.FilePath (takeDirectory)
import System.Exit (ExitCode (..))
import System.FilePath.Glob (glob)
import System.Process (readCreateProcessWithExitCode, proc)
import System.Which (staticWhich)

-- | Resolved at compile time via 'staticWhich' per CLAUDE.md rule.
-- ripgrep must be present in the shell.nix @nativeBuildInputs@ at
-- cabal-build time (gingerly's shell.nix already includes it
-- transitively via nixpkgs' @pkgs.ripgrep@).
ripgrepPath :: FilePath
ripgrepPath = $(staticWhich "rg")

------------------------------------------------------------------------------
-- Tool definitions (names + descriptions verbatim from cli.js)
------------------------------------------------------------------------------

-- | Reads a file from the local filesystem. Description text matches
-- cli.js verbatim; parameter names match cli.js (@file_path@,
-- @offset@, @limit@).
readTool :: ToolDef
readTool = ToolDef
  { _toolDef_name = "Read"
  , _toolDef_description =
      "Reads a file from the local filesystem. The file_path parameter must be \
      \an absolute path, not a relative path. By default, it reads up to 2000 \
      \lines starting from the beginning of the file. You can optionally \
      \specify a line offset and limit, but it's recommended to read the \
      \whole file. Lines longer than 2000 characters will be truncated. \
      \Results are returned using cat -n format, with line numbers starting at 1."
  , _toolDef_inputSchema = Data.Aeson.object
      [ "type" .= ("object" :: T.Text)
      , "properties" .= Data.Aeson.object
          [ "file_path" .= Data.Aeson.object
              [ "type"        .= ("string" :: T.Text)
              , "description" .= ("The absolute path to the file to read" :: T.Text)
              ]
          , "offset" .= Data.Aeson.object
              [ "type"        .= ("number" :: T.Text)
              , "description" .= ("The line number to start reading from. Only provide if the file is too large to read at once." :: T.Text)
              ]
          , "limit" .= Data.Aeson.object
              [ "type"        .= ("number" :: T.Text)
              , "description" .= ("The number of lines to read. Only provide if the file is too large to read at once." :: T.Text)
              ]
          ]
      , "required" .= [ "file_path" :: T.Text ]
      ]
  }

-- | A powerful search tool built on ripgrep. Description matches
-- cli.js verbatim; parameter names match (including the shell-flag
-- style @-i@, @-n@, @-A@, @-B@, @-C@).
grepTool :: ToolDef
grepTool = ToolDef
  { _toolDef_name = "Grep"
  , _toolDef_description =
      "A powerful search tool built on ripgrep. \
      \Supports full regex syntax (e.g., \"log.*Error\", \"function\\\\s+\\\\w+\"). \
      \Filter files with the glob parameter. \
      \Output modes: 'content' shows matching lines, 'files_with_matches' shows \
      \only file paths (default), 'count' shows match counts. \
      \Use head_limit to cap output lines. Multi-line matching requires -U or \
      \patterns containing newlines (not currently exposed)."
  , _toolDef_inputSchema = Data.Aeson.object
      [ "type" .= ("object" :: T.Text)
      , "properties" .= Data.Aeson.object
          [ "pattern" .= stringProp "The regex pattern to search for in file contents"
          , "path"    .= stringProp "File or directory to search in (defaults to CWD)"
          , "glob"    .= stringProp "Glob pattern to filter files (e.g. '*.hs', '**/*.ts')"
          , "output_mode" .= Data.Aeson.object
              [ "type" .= ("string" :: T.Text)
              , "description" .= ("Output mode: 'content' | 'files_with_matches' | 'count'. Defaults to 'files_with_matches'." :: T.Text)
              , "enum" .= (["content", "files_with_matches", "count"] :: [T.Text])
              ]
          , "-i" .= Data.Aeson.object
              [ "type" .= ("boolean" :: T.Text)
              , "description" .= ("Case insensitive search" :: T.Text)
              ]
          , "-n" .= Data.Aeson.object
              [ "type" .= ("boolean" :: T.Text)
              , "description" .= ("Show line numbers in output (requires output_mode=content)" :: T.Text)
              ]
          , "-A" .= Data.Aeson.object
              [ "type" .= ("number" :: T.Text)
              , "description" .= ("Number of lines to show after each match (requires output_mode=content)" :: T.Text)
              ]
          , "-B" .= Data.Aeson.object
              [ "type" .= ("number" :: T.Text)
              , "description" .= ("Number of lines to show before each match (requires output_mode=content)" :: T.Text)
              ]
          , "-C" .= Data.Aeson.object
              [ "type" .= ("number" :: T.Text)
              , "description" .= ("Number of lines of context before AND after each match (requires output_mode=content)" :: T.Text)
              ]
          , "head_limit" .= Data.Aeson.object
              [ "type" .= ("number" :: T.Text)
              , "description" .= ("Limit output to the first N lines/entries" :: T.Text)
              ]
          ]
      , "required" .= [ "pattern" :: T.Text ]
      ]
  }

-- | Writes a file to the local filesystem. Description matches
-- cli.js verbatim; parameter names match (@file_path@, @content@).
--
-- Semantics: overwrites if the file exists. Creates parent
-- directories as needed. Writes are atomic (tempfile + rename).
writeTool :: ToolDef
writeTool = ToolDef
  { _toolDef_name = "Write"
  , _toolDef_description =
      "Writes a file to the local filesystem. Overwrites the existing file if \
      \there is one at the provided path. ALWAYS prefer editing existing files \
      \in the codebase. NEVER create documentation files (*.md) or README \
      \files unless explicitly requested by the User. The file_path parameter \
      \must be an absolute path. Creates parent directories if needed."
  , _toolDef_inputSchema = Data.Aeson.object
      [ "type" .= ("object" :: T.Text)
      , "properties" .= Data.Aeson.object
          [ "file_path" .= stringProp "The absolute path to the file to write"
          , "content"   .= stringProp "The content to write to the file"
          ]
      , "required" .= [ "file_path" :: T.Text, "content" :: T.Text ]
      ]
  }

-- | Batched multi-edit to a single file. Description matches cli.js
-- verbatim; parameter names match (@file_path@, @edits@ where each
-- edit has @old_string@/@new_string@/@replace_all@).
--
-- Semantics: edits are applied in order. If ANY edit fails
-- (old_string not found or ambiguous without replace_all), the
-- WHOLE operation is aborted and no changes are written — atomic.
-- This matches claude-code's own behavior and keeps the file from
-- landing in a half-edited state.
multiEditTool :: ToolDef
multiEditTool = ToolDef
  { _toolDef_name = "MultiEdit"
  , _toolDef_description =
      "This is a tool for making multiple edits to a single file in one \
      \operation. It is built on top of the Edit tool and allows you to perform \
      \multiple find-and-replace operations efficiently. Prefer this tool over \
      \the Edit tool when you need to make multiple edits to the same file. \
      \All edits are applied in sequence; if ANY edit fails (old_string not \
      \found), NO changes are written to the file (atomic). Each edit's \
      \old_string must be unique in the file unless replace_all is true."
  , _toolDef_inputSchema = Data.Aeson.object
      [ "type" .= ("object" :: T.Text)
      , "properties" .= Data.Aeson.object
          [ "file_path" .= stringProp "The absolute path to the file to modify"
          , "edits" .= Data.Aeson.object
              [ "type" .= ("array" :: T.Text)
              , "description" .= ("Array of edit operations. Each edit has old_string, new_string, and optional replace_all." :: T.Text)
              , "items" .= Data.Aeson.object
                  [ "type" .= ("object" :: T.Text)
                  , "properties" .= Data.Aeson.object
                      [ "old_string"  .= stringProp "The text to replace (must be unique in file unless replace_all is true)"
                      , "new_string"  .= stringProp "The replacement text"
                      , "replace_all" .= Data.Aeson.object
                          [ "type" .= ("boolean" :: T.Text)
                          , "description" .= ("Replace ALL occurrences of old_string (default false)" :: T.Text)
                          ]
                      ]
                  , "required" .= [ "old_string" :: T.Text, "new_string" :: T.Text ]
                  ]
              ]
          ]
      , "required" .= [ "file_path" :: T.Text, "edits" :: T.Text ]
      ]
  }

-- | Fast file pattern matching tool. Description matches cli.js
-- verbatim; parameter names match.
globTool :: ToolDef
globTool = ToolDef
  { _toolDef_name = "Glob"
  , _toolDef_description =
      "Fast file pattern matching tool that works with any codebase size. \
      \Supports glob patterns like '**/*.hs' or 'src/**/*.ts'. Returns \
      \matching file paths sorted by modification time (newest first). \
      \Use when you need to find files by name/path pattern (not content — \
      \use Grep for content)."
  , _toolDef_inputSchema = Data.Aeson.object
      [ "type" .= ("object" :: T.Text)
      , "properties" .= Data.Aeson.object
          [ "pattern" .= stringProp "The glob pattern to match files against (e.g. '**/*.hs')"
          , "path"    .= stringProp "The directory to search in. Defaults to current working directory. IMPORTANT: Omit for CWD; do not pass '.' or empty string."
          ]
      , "required" .= [ "pattern" :: T.Text ]
      ]
  }

-- | Small helper: build a @{type:string, description:<d>}@ schema node.
stringProp :: T.Text -> Value
stringProp d = Data.Aeson.object
  [ "type"        .= ("string" :: T.Text)
  , "description" .= d
  ]

------------------------------------------------------------------------------
-- Executors
------------------------------------------------------------------------------

-- | Executes 'readTool'. Semantics:
--
--   * Missing @file_path@ → diagnostic.
--   * File not found → "File not found: <path>" (not an exception).
--   * @offset@ default 1 (1-indexed); @limit@ default 2000.
--   * Lines over 2000 chars → truncated with a marker.
--   * Output format: cli.js-matching @"   <N>\t<content>"@ per line.
execRead :: Value -> IO T.Text
execRead v = case fromJSON v of
  Error e -> pure $ T.pack $ "Invalid Read arguments: " <> e
  Success (Object o) -> case KM.lookup "file_path" o of
    Just (String path) -> do
      let off = case KM.lookup "offset" o of
            Just (Number n) -> max 1 (truncate n) :: Int
            _               -> 1
          lim = case KM.lookup "limit" o of
            Just (Number n) -> max 1 (truncate n) :: Int
            _               -> 2000
          pathStr = T.unpack path
      exists <- doesFileExist pathStr
      if not exists
        then pure $ "File not found: " <> path
        else do
          r <- try (T.readFile pathStr) :: IO (Either SomeException T.Text)
          case r of
            Left ex -> pure $ "Read failed: " <> T.pack (show ex)
            Right contents -> do
              let allLines = T.lines contents
                  windowed = take lim (drop (off - 1) allLines)
                  numbered = zipWith renderLine [off..] windowed
              if null windowed
                then pure $ if null allLines
                              then "<empty file>"
                              else T.pack $ "<offset " <> show off <> " beyond EOF ("
                                              <> show (length allLines) <> " lines total)>"
                else pure $ T.unlines numbered
    _ -> pure "Missing or non-string 'file_path' in Read arguments"
  Success _ -> pure "Expected object for Read arguments"
  where
    renderLine :: Int -> T.Text -> T.Text
    renderLine n line =
      let truncated
            | T.length line > 2000 = T.take 2000 line
                                       <> " [line truncated: "
                                       <> T.pack (show (T.length line - 2000))
                                       <> " chars]"
            | otherwise            = line
          numField = T.justifyRight 6 ' ' (T.pack (show n))
      in numField <> "\t" <> truncated

-- | Executes 'grepTool' via ripgrep. Maps JSON params to CLI args.
execGrep :: Value -> IO T.Text
execGrep v = case fromJSON v of
  Error e -> pure $ T.pack $ "Invalid Grep arguments: " <> e
  Success (Object o) -> case KM.lookup "pattern" o of
    Just (String pat) -> do
      let args = buildGrepArgs o pat
      r <- try (readCreateProcessWithExitCode (proc ripgrepPath args) "")
             :: IO (Either SomeException (ExitCode, String, String))
      case r of
        Left ex -> pure $ "Grep failed: " <> T.pack (show ex)
        Right (ec, out, err) -> case ec of
          ExitSuccess   -> pure $ T.pack out
          ExitFailure 1 -> pure "<no matches>"     -- ripgrep exit 1 = no matches
          ExitFailure n -> pure $ "Grep exit " <> T.pack (show n)
                                    <> (if null err then "" else ": " <> T.pack err)
    _ -> pure "Missing or non-string 'pattern' in Grep arguments"
  Success _ -> pure "Expected object for Grep arguments"
  where
    buildGrepArgs :: KM.KeyMap Value -> T.Text -> [String]
    buildGrepArgs o pat = concat
      [ -- Output mode. ripgrep defaults are useful; map explicitly
        -- when the model specifies.
        case KM.lookup "output_mode" o of
          Just (String "files_with_matches") -> ["--files-with-matches"]
          Just (String "count")              -> ["--count"]
          Just (String "content")            -> []   -- ripgrep default
          _                                  -> ["--files-with-matches"]
      , boolFlag "-i" o
      , boolFlag "-n" o
      , intFlag "-A" o
      , intFlag "-B" o
      , intFlag "-C" o
      , case KM.lookup "glob" o of
          Just (String g) -> ["--glob", T.unpack g]
          _               -> []
      , case KM.lookup "head_limit" o of
          Just (Number n) -> ["--max-count", show (truncate n :: Int)]
          _               -> []
      , [T.unpack pat]
      , case KM.lookup "path" o of
          Just (String p) -> [T.unpack p]
          _               -> ["."]
      ]
    boolFlag :: String -> KM.KeyMap Value -> [String]
    boolFlag k o =
      case KM.lookup (fromString k) o of
        Just (Bool True) -> [k]
        _                -> []
    intFlag :: String -> KM.KeyMap Value -> [String]
    intFlag k o =
      case KM.lookup (fromString k) o of
        Just (Number n) -> [k, show (truncate n :: Int)]
        _               -> []
    fromString :: String -> KM.Key
    fromString = Key.fromString

-- | Executes 'globTool' via the pure Haskell @System.FilePath.Glob@
-- library. Resolves patterns relative to @path@ (default CWD),
-- sorts results by modification time descending.
execGlob :: Value -> IO T.Text
execGlob v = case fromJSON v of
  Error e -> pure $ T.pack $ "Invalid Glob arguments: " <> e
  Success (Object o) -> case KM.lookup "pattern" o of
    Just (String pat) -> do
      let patStr = T.unpack pat
          basePath = case KM.lookup "path" o of
            Just (String p) | not (T.null (T.strip p)) -> T.unpack p <> "/"
            _                                           -> ""
          fullPat = basePath <> patStr
      r <- try (glob fullPat) :: IO (Either SomeException [FilePath])
      case r of
        Left ex -> pure $ "Glob failed: " <> T.pack (show ex)
        Right matches
          | null matches -> pure "<no matches>"
          | otherwise    -> do
              -- Sort by mtime desc (newest first), matching claude-code.
              withTimes <- mapM (\p -> do
                mt <- try (getModificationTime p)
                        :: IO (Either SomeException UTCTime)
                pure (p, either (const Nothing) Just mt)) matches
              let sorted = sortBy (comparing (Down . snd)) withTimes
              pure $ T.unlines (map (T.pack . fst) sorted)
    _ -> pure "Missing or non-string 'pattern' in Glob arguments"
  Success _ -> pure "Expected object for Glob arguments"

-- | Executes 'writeTool'. Atomic write via tempfile + rename; creates
-- parent directories as needed. Returns a diagnostic summary with the
-- number of bytes written. Silently overwrites.
execWrite :: Value -> IO T.Text
execWrite v = case fromJSON v of
  Error e -> pure $ T.pack $ "Invalid Write arguments: " <> e
  Success (Object o) -> case (KM.lookup "file_path" o, KM.lookup "content" o) of
    (Just (String path), Just (String content)) -> do
      let pathStr = T.unpack path
          dir     = takeDir pathStr
      _ <- try (createDirectoryIfMissing True dir) :: IO (Either SomeException ())
      let tmp = pathStr <> ".gingerly-write-tmp"
      r <- try (do
                   T.writeFile tmp content
                   renameFile tmp pathStr)
             :: IO (Either SomeException ())
      case r of
        Left ex -> pure $ "Write failed: " <> T.pack (show ex)
        Right () -> pure $
          "Wrote " <> T.pack (show (T.length content)) <> " chars to " <> path
    _ -> pure "Missing file_path or content in Write arguments"
  Success _ -> pure "Expected object for Write arguments"
  where
    -- Minimal parent-dir extractor (FilePath's takeDirectory lives in
    -- System.FilePath which we already have).
    takeDir :: FilePath -> FilePath
    takeDir = takeDirectory

-- | Executes 'multiEditTool'. Atomic: reads the file ONCE, applies
-- the edits sequentially to the in-memory text, and if ALL succeed
-- writes back atomically. If any edit's old_string is not found
-- (or found multiple times without replace_all=true), the WHOLE
-- operation fails with no disk writes. Mirrors cli.js's behavior.
execMultiEdit :: Value -> IO T.Text
execMultiEdit v = case fromJSON v of
  Error e -> pure $ T.pack $ "Invalid MultiEdit arguments: " <> e
  Success (Object o) ->
    case (KM.lookup "file_path" o, KM.lookup "edits" o) of
      (Just (String path), Just (Array editsArr)) -> do
        let pathStr = T.unpack path
        exists <- doesFileExist pathStr
        if not exists
          then pure $ "File not found: " <> path
          else do
            r <- try (T.readFile pathStr) :: IO (Either SomeException T.Text)
            case r of
              Left ex -> pure $ "MultiEdit read failed: " <> T.pack (show ex)
              Right original -> do
                let editsList = toList editsArr
                case applyEdits original editsList 0 of
                  Left err -> pure err
                  Right newContent -> do
                    let tmp = pathStr <> ".gingerly-medit-tmp"
                    w <- try (do
                                 T.writeFile tmp newContent
                                 renameFile tmp pathStr)
                           :: IO (Either SomeException ())
                    case w of
                      Left ex -> pure $ "MultiEdit write failed: " <> T.pack (show ex)
                      Right () -> pure $
                        "Applied " <> T.pack (show (length editsList))
                           <> " edits to " <> path
      _ -> pure "Missing file_path or edits in MultiEdit arguments"
  Success _ -> pure "Expected object for MultiEdit arguments"
  where
    applyEdits :: T.Text -> [Value] -> Int -> Either T.Text T.Text
    applyEdits acc []       _ = Right acc
    applyEdits acc (e:rest) i = case e of
      Object eo ->
        case (KM.lookup "old_string" eo, KM.lookup "new_string" eo) of
          (Just (String oldS), Just (String newS)) ->
            let replaceAll = case KM.lookup "replace_all" eo of
                  Just (Bool True) -> True
                  _                -> False
            in case applyOne acc oldS newS replaceAll of
                 Left err -> Left $
                   "Edit #" <> T.pack (show (i+1)) <> " failed: " <> err
                 Right acc' -> applyEdits acc' rest (i+1)
          _ -> Left $
            "Edit #" <> T.pack (show (i+1))
              <> " missing or non-string old_string/new_string"
      _ -> Left $
        "Edit #" <> T.pack (show (i+1)) <> " not a JSON object"

    applyOne :: T.Text -> T.Text -> T.Text -> Bool -> Either T.Text T.Text
    applyOne content oldS newS replaceAll
      | T.null oldS = Left "old_string is empty"
      | not (oldS `T.isInfixOf` content) =
          Left ("old_string not found in file")
      | replaceAll = Right (T.replace oldS newS content)
      | otherwise =
          let occurrences = countOccurrences oldS content
          in if occurrences > 1
             then Left $ "old_string appears "
                           <> T.pack (show occurrences)
                           <> " times — pass replace_all=true or disambiguate"
             else Right (T.replace oldS newS content)

    countOccurrences :: T.Text -> T.Text -> Int
    countOccurrences needle haystack
      | T.null needle = 0
      | otherwise     = length (T.breakOnAll needle haystack)
