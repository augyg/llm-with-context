{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- Capability instances live here (where the provider's runner is), not in
-- 'LLM.Capability' (where the class is) or 'LLM.Types' (where the kind is).
-- The split is intentional: each provider's instance is provider-specific.
{-# OPTIONS_GHC -Wno-orphans #-}

-- | @'AnthropicCli@ provider — the local @claude@ binary.
--
-- The CLI is what the SimpleHaskellRecordings pipeline uses by default:
-- it's billed under a flat subscription (no per-token charge) and it
-- builds multimodal requests from file paths embedded in the prompt.
-- Both properties make it the right transport for the project's many
-- vision-driven asks.
--
-- This module ports the relevant pieces of the standalone @claude@
-- package into 'llm-with-context' and wires them up as @'AnthropicCli@
-- 'CanText' / 'CanMultimodal' instances. Two image-attachment
-- mechanics live here:
--
--   1. The @--add-dir=<DIR>@ EQUALS-BOUND argv form, granting Claude's
--      Read tool access to each image's parent directory. The CLI's
--      argparse treats the spaced form @--add-dir <DIR>@ as variadic
--      and greedily consumes the following positional prompt as
--      another directory — the equals-bound form scopes the value to
--      the flag explicitly. This is load-bearing for multimodal.
--
--   2. The 'readPreamble' helper, auto-prepended in front of the user
--      prompt, that explicitly instructs Claude to invoke its Read
--      tool on each image path. Empirically a passive \"the image
--      at \<path\>...\" mention is not enough; the verb \"Read\" plus
--      an explicit \"using your Read tool\" cue is what makes the
--      call multimodal in practice.
module LLM.Provider.AnthropicCli
  ( -- * Typed CLI args
    AskArgs (..)
  , defaultAskArgs
  , askArgs
    -- * Multimodal helper
  , readPreamble
  , multimodalAskArgs
    -- * Effect runner
  , runLLMAnthropicCli
  ) where

import qualified Control.Exception as CE
import Data.List (nub)
import qualified Data.Text as T
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory)
import System.Process
  ( CreateProcess (env)
  , proc
  , readCreateProcessWithExitCode
  )
import System.Which (staticWhich)

import Effectful (Eff, IOE, liftIO, (:>))
import Effectful.Dispatch.Dynamic (interpret, send)

import LLM.Capability
  ( CanJsonOutput (..)
  , CanMultimodal (..)
  , CanText (..)
  , ImageInput
  , TransportError (..)
  )
import Data.Bifunctor (first)
import LLM.Effect (LLM (..))
import LLM.Effect.Memory (Memory)
import LLM.Types
  ( APIProvider (..)
  , ContentWithRole (..)
  , GPTRole (User)
  , ToolTurn (..)
  )


-- | A typed @claude@ CLI ask invocation.
data AskArgs = AskArgs
  { aPrompt    :: !T.Text
    -- ^ The text prompt. May contain file paths the CLI reads inline
    -- via Claude's internal tool-use logic.
  , aPrintMode :: !Bool
    -- ^ Adds @-p@ (non-interactive print mode).
  , aSkipPerms :: !Bool
    -- ^ Adds @--dangerously-skip-permissions@.
  , aScrubEnv  :: ![String]
    -- ^ Names of environment variables to strip from the inherited
    -- process environment before invoking the CLI. Defaults to
    -- @["CLAUDECODE"]@ to prevent Claude Code from refusing to recurse
    -- into itself when running inside an outer Claude Code session.
  , aAddDirs   :: ![FilePath]
    -- ^ Directories to grant tool access to via repeated
    -- @--add-dir=<DIR>@ flags (EQUALS-BOUND — see module header).
  } deriving (Show, Eq)

-- | Sensible defaults for an LLM ask: print mode on, skip permissions
-- on, env-scrub @CLAUDECODE@, no extra directory access.
defaultAskArgs :: T.Text -> AskArgs
defaultAskArgs p = AskArgs
  { aPrompt    = p
  , aPrintMode = True
  , aSkipPerms = True
  , aScrubEnv  = ["CLAUDECODE"]
  , aAddDirs   = []
  }

-- | Build the argv slice. The caller prepends the @claude@ binary
-- path via 'runLLMAnthropicCli' / 'staticWhich'.
askArgs :: AskArgs -> [String]
askArgs (AskArgs prompt printMode skipPerms _ addDirs) =
     printArgs
  <> skipPermsArgs
  <> addDirArgs
  <> [ T.unpack prompt ]
  where
    printArgs     = [ "-p"                              | printMode ]
    skipPermsArgs = [ "--dangerously-skip-permissions"  | skipPerms ]
    -- See module header: equals-bind each --add-dir value because the
    -- CLI's argparse otherwise greedily consumes the following prompt.
    addDirArgs    = map (\d -> "--add-dir=" <> d) addDirs

-- | Build the auto-prepended \"Read the image(s) at ...\" preamble for
-- multimodal asks. Exported for test inspection.
--
--   * @[]@      → empty (no preamble).
--   * @[p]@     → @"Read the image file at <p> using your Read tool.\n\n"@.
--   * @[p1,p2…]@→ a bullet list \"Read the following image files using
--                 your Read tool:\\n  - p1\\n  - p2\\n…\\n\".
readPreamble :: [FilePath] -> T.Text
readPreamble = \case
  []  -> ""
  [p] ->
    "Read the image file at " <> T.pack p <> " using your Read tool.\n\n"
  ps  ->
       "Read the following image files using your Read tool:\n"
    <> T.unlines [ "  - " <> T.pack p | p <- ps ]
    <> "\n"

-- | Shell out to the @claude@ binary with the typed args. The path is
-- resolved at COMPILE time via 'staticWhich' so we get a determinate
-- /nix/store path baked in — the build pulls 'claude-code' from a
-- pinned newer-than-24.11 nixpkgs (see default.nix's 'newerPkgs').
runClaudeAsk :: AskArgs -> IO (Either T.Text T.Text)
runClaudeAsk a = do
  let argv = askArgs a
      scrub = aScrubEnv a
  curEnv <- getEnvironment
  let cleanEnv = filter (\(k, _) -> k `notElem` scrub) curEnv
      cp = (proc $(staticWhich "claude") argv)
             { env = Just cleanEnv }
  result <- CE.try (readCreateProcessWithExitCode cp "")
              :: IO (Either CE.SomeException (ExitCode, String, String))
  pure $ case result of
    Left e -> Left (T.pack ("claude spawn failed: " <> show e))
    Right (ExitSuccess, out, _) ->
      let trimmed = T.strip (T.pack out)
      in if T.null trimmed
           then Left "claude returned empty response"
           else Right trimmed
    Right (ExitFailure n, _, err) ->
      Left (T.pack ("claude exited " <> show n <> ": " <> take 400 err))

-- | Flatten a 'ContentWithRole' list into a single prompt body — the
-- CLI only takes one positional prompt argument.
flattenContents :: [ContentWithRole] -> T.Text
flattenContents = T.intercalate "\n\n" . map _cwr_content

-- | Build the typed 'AskArgs' for a multimodal CLI ask. Pure helper so
-- the argv shape is testable without invoking the @claude@ binary:
--   * @--add-dir=<DIR>@ per unique parent directory ('nub'-deduped);
--   * 'readPreamble' prepended in front of the flattened user prompt;
--   * default print-mode + skip-permissions front matter (via
--     'defaultAskArgs').
multimodalAskArgs :: [FilePath] -> [ContentWithRole] -> AskArgs
multimodalAskArgs paths contents =
  let dirs     = nub (map takeDirectory paths)
      preamble = readPreamble paths
      body     = preamble <> flattenContents contents
  in (defaultAskArgs body) { aAddDirs = dirs }

-- | Interpret @LLM 'AnthropicCli@ against the real @claude@ binary.
-- The interpreter handles the generic 'Ask' op; the capability classes
-- ('CanText', 'CanMultimodal') sit on top of 'Ask' as more typed entry
-- points.
runLLMAnthropicCli
  :: forall es a. (IOE :> es, Memory :> es)
  => Eff (LLM 'AnthropicCli : es) a
  -> Eff es a
runLLMAnthropicCli = interpret $ \_ -> \case
  Ask contents -> liftIO $ runClaudeAsk (defaultAskArgs (flattenContents contents))
  -- Multimodal: derive each path's parent dir, prepend the Read-tool
  -- preamble in front of the flattened user prompt, and bind --add-dir
  -- per unique dir (EQUALS-BOUND, see module header). The result
  -- routes through the same 'runClaudeAsk' shellout as 'Ask', so
  -- cross-cutting middleware observes both uniformly.
  AskMultimodal paths contents -> liftIO $ runClaudeAsk (multimodalAskArgs paths contents)
  -- Typed / context / tools paths are not modelled at the CLI yet;
  -- the capability classes give the supported intents typed entry
  -- points. These three deliberately throw at runtime so a consumer
  -- that drifts onto them at the generic LLM-effect level surfaces a
  -- specific message instead of silent default-shaped output.
  AskTyped _ -> error
    "LLM.Provider.AnthropicCli: AskTyped is not implemented \
    \(use askText / askWithImages from LLM.Capability instead)."
  AskWithContext _ _ -> error
    "LLM.Provider.AnthropicCli: AskWithContext is not implemented \
    \(use askText / askWithImages from LLM.Capability instead)."
  AskWithContextTyped _ _ -> error
    "LLM.Provider.AnthropicCli: AskWithContextTyped is not implemented \
    \(use askText / askWithImages from LLM.Capability instead)."
  AskTools _ _ -> pure (Right (ToolTurn (Just "") [] []))


-- | Text-only ask via @claude -p PROMPT@. Wraps a single @User@ turn
-- and dispatches via the underlying 'Ask' op; transport-layer failures
-- bubble back as @'Left' 'TransportError'@ for the consumer to handle
-- (crash, retry, fall back) per the honest-API rule.
instance CanText 'AnthropicCli where
  askText prompt = do
    res <- send (Ask [ContentWithRole User prompt] :: LLM 'AnthropicCli (Eff es) (Either T.Text T.Text))
    pure (first TransportError res)

-- | 'ImageInput' carrier for the CLI: the raw list of image file paths
-- to grant Read-tool access to. Pinned as a top-level @type instance@
-- (the family lives in "LLM.Effect.ImageInput") so the 'AskMultimodal'
-- GADT constructor can carry it as a typed field at the effect layer.
type instance ImageInput 'AnthropicCli = [FilePath]

-- | Multimodal ask: route through the 'AskMultimodal' GADT
-- constructor via 'send', so the framework's middleware (Budget,
-- Retry, Memory, Log) observes the call the same way it observes
-- text-only 'Ask' calls. The CLI-specific @--add-dir=<DIR>@ + Read-
-- tool preamble mechanics live in the interpreter
-- ('runLLMAnthropicCli'), not here.
--
-- Transport-layer failures bubble as @'Left' 'TransportError'@ — same
-- shape as 'CanText.askText'. The lower-level @Either T.Text T.Text@
-- path is 'askMultimodal' from "LLM.Effect" if a consumer wants the
-- raw text error string instead of the typed newtype.
instance CanMultimodal 'AnthropicCli where
  askWithImages paths userPrompt = do
    res <- send (AskMultimodal paths [ContentWithRole User userPrompt]
                  :: LLM 'AnthropicCli (Eff es) (Either T.Text T.Text))
    pure (first TransportError res)

-- | Structured-output ask via the @claude@ CLI. The CLI has no native
-- @--json-schema@ flag the way the HTTP API has @response_format@, so
-- the schema is delivered to Claude prompt-side: the schema text (a
-- 'String' carrying the JSON example shape — see "LLM.JsonExample" /
-- 'jsonResponsePrompt') is prepended to the user prompt with a clear
-- "Respond with ONLY a JSON object matching this shape:" header.
--
-- The result is the raw response text — parsing is the consumer's
-- problem (honest-API rule: parse errors bubble up at the call site,
-- they are never swallowed inside the instance, and there is no
-- in-instance retry). For end-to-end JSON ergonomics on top of this,
-- consumers compose 'askJson' with their own parser + retry policy.
--
-- The dispatch routes through the same 'Ask' GADT constructor as
-- 'askText', so middleware (Budget, Retry, Memory, Log) observes JSON
-- asks the same way it observes plain text asks.
instance CanJsonOutput 'AnthropicCli where
  type Schema 'AnthropicCli = String
  askJson schema userPrompt = do
    let body = "Respond with ONLY a JSON object matching this shape:\n"
             <> T.pack schema
             <> "\n\n"
             <> userPrompt
    res <- send (Ask [ContentWithRole User body]
                  :: LLM 'AnthropicCli (Eff es) (Either T.Text T.Text))
    pure (first TransportError res)
