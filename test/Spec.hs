{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Pure tests for the effect layer. Everything here runs via 'runPureEff' —
-- no network, no IO — by interpreting the LLM effect with the mock backend and
-- Memory/Tool over their pure interpreters. Dependency-light on purpose (just
-- base/text/effectful-core/the lib): a hand-rolled harness, no test framework.
module Main (main) where

import Control.Monad (forM_, unless)
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (exitFailure)

import Effectful (runPureEff)
import Effectful.State.Static.Local (evalState)

import qualified Control.Exception as CE
import LLM.Capability (askText)
import LLM.Effect (askMultimodal, askWithContext)
import LLM.Provider.AnthropicCli
  ( AskArgs (..)
  , askArgs
  , defaultAskArgs
  , multimodalAskArgs
  , readPreamble
  )
-- Importing these brings the real HTTP capability instances into scope
-- so the shape / smoke tests can typecheck. The modules themselves
-- export nothing other than their instances.
import LLM.Provider.AnthropicHttp ()
import LLM.Provider.OpenAIHttp ()
import qualified LLM.Provider.Anthropic as A
import qualified LLM.Provider.OpenAI as O
import LLM.Effect.Anthropic (runLLMAnthropic)
import LLM.Effect.OpenAI (runLLMOpenAI)
import LLM.Types (APIKey (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson (Parser, parseEither, parseMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment (lookupEnv)
import LLM.Effect.Memory
  ( emptyMemoryStore
  , pin
  , prune
  , recallAll
  , remember
  , runMemoryState
  )
import Data.IORef (newIORef, readIORef)
import LLM.Effect.Mock (runLLMMock, runLLMMockRecording)
import Effectful (runEff)
import LLM.Effect.Tool
  ( ShellCommand (..)
  , ToolHandlers (..)
  , ToolResult (..)
  , defaultRunOptions
  , defaultToolHandlers
  , renderShellCommand
  , runCommand
  , runToolWith
  )
import LLM.Effect.Tool.Sandbox
  ( bubblewrapArgv
  , defaultBWrapConfig
  , defaultSbExecConfig
  , jail
  , sandboxExecProfile
  , shellCommandPaths
  )
import Data.List (isInfixOf)
import LLM.Types
  ( APIProvider (..)
  , ContentWithRole (..)
  , ConvoAnswer (..)
  , ConvoQuery (..)
  , ConvoQuestion (..)
  , GPTRole (User)
  , RelevantContext (..)
  , Tag (..)
  , cwr
  )

main :: IO ()
main = do
  -- The HTTP capability instances now delegate via 'send (Ask ...)' to
  -- the active LLM interpreter. Pointing them at the mock backend
  -- routes the prompt through and returns the canned response — no
  -- network, no throw.
  let anthropicProbe :: Text
      anthropicProbe = runPureEff
        . evalState emptyMemoryStore
        . runMemoryState
        . runLLMMock @'AnthropicHttp (\_ -> Right "ok-anthropic")
        $ askText @'AnthropicHttp "x"
      openAIProbe    :: Text
      openAIProbe    = runPureEff
        . evalState emptyMemoryStore
        . runMemoryState
        . runLLMMock @'OpenAIHttp (\_ -> Right "ok-openai")
        $ askText @'OpenAIHttp "x"
  liveSmokeChecks <- runLiveSmokeChecks
  recordingAskCapturesPrompt   <- recordingAskTest
  recordingMultimodalCapturesText <- recordingMultimodalTest
  let checks =
        [ ("memory: remember/recallAll round-trip", memCount == 2)
        , ("memory: pinned turn survives prune", pinnedTags == ["keep"])
        , ("effect: context loop records each turn", ctxTurns == 2)
        , ("tool: runCommand with pure handlers", toolStdout == "ls")
        , ("sandbox: jail accepts relative, rejects absolute + ..", jailOk)
        , ("sandbox: shellCommandPaths extracts the path", shellCommandPaths (Grep "x" "sub/dir" False False) == ["sub/dir"])
        , ("sandbox: bwrap argv binds root + ro-binds store + runs exe", bwrapArgvOk)
        , ("sandbox: sandbox-exec profile confines writes to root", sbProfileOk)
        , ("anthropic-cli: readPreamble [] is empty", readPreamble [] == "")
        , ("anthropic-cli: readPreamble [p] uses Read tool wording"
          , readPreamble ["/img/a.png"] == "Read the image file at /img/a.png using your Read tool.\n\n")
        , ("anthropic-cli: readPreamble [p1,p2] enumerates as bullets", multiPreambleOk)
        , ("anthropic-cli: askArgs emits --add-dir=DIR (equals-bound)", addDirEqualsBound)
        , ("anthropic-cli: askArgs prepends -p and skip-perms", argvFrontMatter)
        , ("anthropic-http: askText routes through interpreter (mock)", anthropicProbe == "ok-anthropic")
        , ("openai-http: askText routes through interpreter (mock)", openAIProbe == "ok-openai")
        , ("anthropic-http: detectImageMediaType identifies PNG magic", A.detectImageMediaType pngMagic == "image/png")
        , ("anthropic-http: detectImageMediaType identifies JPEG magic", A.detectImageMediaType jpegMagic == "image/jpeg")
        , ("anthropic-http: detectImageMediaType defaults to PNG on unknown bytes", A.detectImageMediaType "????" == "image/png")
        , ("anthropic-http: multimodal body has model/max_tokens/messages", anthropicBodyShapeOk)
        , ("anthropic-http: multimodal body base64-encodes image bytes", anthropicBodyBase64Ok)
        , ("anthropic-http: multimodal body places text block AFTER image blocks", anthropicBodyOrderOk)
        , ("openai-http: multimodal body has model/max_tokens/messages", openaiBodyShapeOk)
        , ("openai-http: multimodal body carries image_url parts", openaiBodyImageUrlOk)
        , ("openai-http: multimodal body places text part AFTER image parts", openaiBodyOrderOk)
        , ("anthropic-http: 1x1 PNG fixture decodes to bytes starting with PNG magic", onePixelPngIsPng)
        , ("anthropic-cli: multimodalAskArgs binds --add-dir per unique parent dir", multimodalArgsDirsOk)
        , ("anthropic-cli: multimodalAskArgs prepends Read-tool preamble to prompt", multimodalArgsPreambleOk)
        , ("effect: AskMultimodal routes through Mock backend's responder", askMultimodalRoutesOk)
        , ("mock-recording: Ask is recorded with original contents", recordingAskCapturesPrompt)
        , ("mock-recording: AskMultimodal records text turns (no image carrier)", recordingMultimodalCapturesText)
        ]
        <> liveSmokeChecks
  forM_ checks $ \(name, ok) ->
    putStrLn $ (if ok then "PASS  " else "FAIL  ") <> name
  unless (all snd checks) exitFailure

-- Two remembered turns should both be recallable.
memCount :: Int
memCount = runPureEff . evalState emptyMemoryStore . runMemoryState $ do
  remember (ConvoQuery (Tag "a") (ConvoQuestion []) (ConvoAnswer "first"))
  remember (ConvoQuery (Tag "b") (ConvoQuestion []) (ConvoAnswer "second"))
  length <$> recallAll

-- prune 0 drops everything unpinned; the pinned turn must remain.
pinnedTags :: [Text]
pinnedTags = runPureEff . evalState emptyMemoryStore . runMemoryState $ do
  remember (ConvoQuery (Tag "keep") (ConvoQuestion []) (ConvoAnswer "x"))
  pin (Tag "keep")
  remember (ConvoQuery (Tag "t1") (ConvoQuestion []) (ConvoAnswer "y"))
  remember (ConvoQuery (Tag "t2") (ConvoQuestion []) (ConvoAnswer "y"))
  prune 0
  map (unTag . _convoQuery_tag) <$> recallAll

-- Two context-aware asks against the mock provider should leave two turns in
-- memory. Fully pure: mock LLM + Memory + State, run with runPureEff.
ctxTurns :: Int
ctxTurns =
  runPureEff
    . evalState emptyMemoryStore
    . runMemoryState
    . runLLMMock @'AnthropicHttp (\_ -> Right "ok")
    $ do
        _ <- askWithContext @'AnthropicHttp (LastN 10) (Tag "q1", ConvoQuestion [cwr User "hi"])
        _ <- askWithContext @'AnthropicHttp (LastN 10) (Tag "q2", ConvoQuestion [cwr User "again"])
        length <$> recallAll

-- A custom (pure) Tool backend: stdout is just the rendered command name.
-- Uses defaultToolHandlers so only the one handler we exercise is overridden.
toolStdout :: Text
toolStdout = runPureEff . runToolWith handlers $ do
  result <- runCommand (Ls "." True False)
  pure (_toolResult_stdout result)
  where
    handlers =
      defaultToolHandlers
        { _toolHandlers_runCommand = \_opts cmd -> pure (ToolResult 0 (fst (renderShellCommand cmd)) "")
        }

-- Sandbox path jail: relative paths pass (rebased under root); absolute paths
-- and '..' traversal are rejected.
jailOk :: Bool
jailOk =
  jail "/root" "sub/file.txt" == Right "/root/sub/file.txt"
    && isLeft (jail "/root" "/etc/passwd")
    && isLeft (jail "/root" "../escape")
  where
    isLeft = either (const True) (const False)

-- The bwrap argv must bind the sandbox root at the project mount, ro-bind the
-- configured store path, and end by invoking the rendered command.
bwrapArgvOk :: Bool
bwrapArgvOk =
  all (`elem` argv) ["--bind", "/srv/root", "/project", "--ro-bind", "/nix/store", "--chdir", "ls"]
  where
    argv = bubblewrapArgv defaultBWrapConfig "/srv/root" "/tmp/box" "/usr/bin:/bin"
             defaultRunOptions (renderShellCommand (Ls "." True False))

-- The generated Seatbelt profile must confine writes to the root and (with the
-- default config) deny network.
sbProfileOk :: Bool
sbProfileOk =
  ("(allow file-write* (subpath \"/srv/root\"))" `isInfixOf` profile)
    && not ("(allow network*)" `isInfixOf` profile)
  where
    profile = sandboxExecProfile defaultSbExecConfig "/srv/root"

-- A two-path preamble must enumerate the paths as a bullet list inside the
-- "Read the following image files" header.
multiPreambleOk :: Bool
multiPreambleOk =
  let p = readPreamble ["/a/x.png", "/a/y.png"]
  in "Read the following image files" `T.isInfixOf` p
       && "  - /a/x.png" `T.isInfixOf` p
       && "  - /a/y.png" `T.isInfixOf` p
  where
    -- shadowed import to keep this self-contained without changing the
    -- top of the file
    _shadow = ()

-- Two-path askArgs must emit BOTH --add-dir flags in the equals-bound form
-- ("--add-dir=DIR"), never as separate "--add-dir" "DIR" tokens, because the
-- CLI's argparse otherwise greedily consumes the following positional prompt.
addDirEqualsBound :: Bool
addDirEqualsBound =
  let a   = (defaultAskArgs "hello") { aAddDirs = ["/srv/a", "/srv/b"] }
      av  = askArgs a
  in    "--add-dir=/srv/a" `elem` av
     && "--add-dir=/srv/b" `elem` av
     && "--add-dir" `notElem` av  -- bare form must not appear
     && last av == "hello"        -- prompt is last positional

-- The default args must put -p and --dangerously-skip-permissions in front of
-- the prompt body.
argvFrontMatter :: Bool
argvFrontMatter =
  let av = askArgs (defaultAskArgs "hello")
  in take 2 av == ["-p", "--dangerously-skip-permissions"]
       && last av == "hello"

-- Two-image multimodal: --add-dir must appear once per unique parent dir
-- (equals-bound), in the same order as nub-deduped takeDirectory.
multimodalArgsDirsOk :: Bool
multimodalArgsDirsOk =
  let a   = multimodalAskArgs ["/srv/a/1.png", "/srv/a/2.png", "/srv/b/3.png"] [cwr User "go"]
      av  = askArgs a
  in    "--add-dir=/srv/a" `elem` av
     && "--add-dir=/srv/b" `elem` av
     && length (filter (== "--add-dir=/srv/a") av) == 1
     && aAddDirs a == ["/srv/a", "/srv/b"]

-- The constructed prompt body must lead with the Read-tool preamble and
-- carry the flattened user contents after it.
multimodalArgsPreambleOk :: Bool
multimodalArgsPreambleOk =
  let a = multimodalAskArgs ["/img/x.png"] [cwr User "describe please"]
  in    "Read the image file at /img/x.png using your Read tool." `T.isInfixOf` aPrompt a
     && "describe please" `T.isInfixOf` aPrompt a

-- Routing 'askMultimodal' through the Mock interpreter must reach the
-- responder with the text contents intact (the image carrier is dropped
-- by the mock, by design — the assertion is on the text path).
askMultimodalRoutesOk :: Bool
askMultimodalRoutesOk =
  let result = runPureEff
        . evalState emptyMemoryStore
        . runMemoryState
        . runLLMMock @'AnthropicCli (\contents ->
            if any (\c -> "frame.png" `T.isInfixOf` _cwr_content c) contents
              then Right "saw-frame"
              else Right "no-frame")
        $ askMultimodal @'AnthropicCli ["/img/frame.png"] [cwr User "frame.png is the image"]
  in result == Right "saw-frame"

-- Recording variant: an 'Ask' against the recording mock must append
-- the exact prompt contents to the sink, and the canned responder's
-- text must still come back to the caller.
recordingAskTest :: IO Bool
recordingAskTest = do
  sink <- newIORef []
  let promptText = "what is the capital of France?"
  answer <-
    runEff
      . evalState emptyMemoryStore
      . runMemoryState
      . runLLMMockRecording @'AnthropicCli sink (\_ -> Right "Paris")
      $ askText @'AnthropicCli promptText
  recorded <- reverse <$> readIORef sink
  pure $ answer == "Paris"
       && length recorded == 1
       && case recorded of
            (cs:_) -> any (\c -> _cwr_content c == promptText) cs
            []     -> False

-- Recording + multimodal: image carrier is dropped (matches
-- 'runLLMMock' behaviour), text turns ARE recorded.
recordingMultimodalTest :: IO Bool
recordingMultimodalTest = do
  sink <- newIORef []
  let userText = "describe this frame"
  result <-
    runEff
      . evalState emptyMemoryStore
      . runMemoryState
      . runLLMMockRecording @'AnthropicCli sink (\_ -> Right "a cat")
      $ askMultimodal @'AnthropicCli ["/img/frame.png"] [cwr User userText]
  recorded <- reverse <$> readIORef sink
  pure $ result == Right "a cat"
       && length recorded == 1
       && case recorded of
            (cs:_) -> any (\c -> _cwr_content c == userText) cs
            []     -> False

-- =========================================================================
-- HTTP multimodal request-body shape tests (pure, no network).
-- =========================================================================

-- Standard PNG file signature (8-byte magic).
pngMagic :: BS.ByteString
pngMagic = BS.pack [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]

-- Standard JPEG SOI + APP0/APP1 marker prefix.
jpegMagic :: BS.ByteString
jpegMagic = BS.pack [0xFF, 0xD8, 0xFF, 0xE0]

-- Minimal valid 1x1 PNG — round-trip test: base64-decoding then encoding
-- the bytes should equal the original base64.
onePixelPng :: BS.ByteString
onePixelPng =
  let b64 = "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNgAAIAAAUAAarVyFEAAAAASUVORK5CYII="
  in case B64.decode (TE.encodeUtf8 b64) of
       Right bs -> bs
       Left e   -> error ("test: cannot decode onePixelPng base64: " <> e)

-- The decoded bytes must start with the PNG magic.
onePixelPngIsPng :: Bool
onePixelPngIsPng = BS.take 8 onePixelPng == pngMagic

-- Anthropic body has the three required top-level keys.
anthropicBodyShapeOk :: Bool
anthropicBodyShapeOk =
  let v = A.multimodalRequestBody "claude-test" 1024 [pngMagic] "hi"
  in case Aeson.parseEither (Aeson.withObject "body" $ \o -> do
                                _ <- o Aeson..: "model"      :: Aeson.Parser Text
                                _ <- o Aeson..: "max_tokens" :: Aeson.Parser Int
                                _ <- o Aeson..: "messages"   :: Aeson.Parser Aeson.Value
                                pure ()) v of
       Right () -> True
       Left _   -> False

-- Anthropic body base64-encodes the bytes inside source.data.
anthropicBodyBase64Ok :: Bool
anthropicBodyBase64Ok =
  let v = A.multimodalRequestBody "claude-test" 1024 [pngMagic] "hi"
      expectedB64 = TE.decodeUtf8 (B64.encode pngMagic)
      -- Drill: body.messages[0].content[0].source.data
      dataField = Aeson.parseMaybe (Aeson.withObject "body" $ \o -> do
        msgs    <- o Aeson..: "messages"
        case (msgs :: [Aeson.Value]) of
          (m:_) -> flip (Aeson.withObject "msg") m $ \mo -> do
            cs <- mo Aeson..: "content"
            case (cs :: [Aeson.Value]) of
              (c:_) -> flip (Aeson.withObject "block") c $ \bo -> do
                src <- bo Aeson..: "source"
                Aeson.withObject "src" (\so -> so Aeson..: "data") src
              [] -> fail "no content"
          [] -> fail "no msgs") v
  in dataField == Just expectedB64

-- The text block must come AFTER the image blocks (model attention order).
anthropicBodyOrderOk :: Bool
anthropicBodyOrderOk =
  let v = A.multimodalRequestBody "claude-test" 1024 [pngMagic] "the-prompt"
      typeList = Aeson.parseMaybe (Aeson.withObject "body" $ \o -> do
        msgs <- o Aeson..: "messages"
        case (msgs :: [Aeson.Value]) of
          (m:_) -> flip (Aeson.withObject "msg") m $ \mo -> do
            cs <- mo Aeson..: "content"
            traverse (Aeson.withObject "block" (\bo -> bo Aeson..: "type")) (cs :: [Aeson.Value])
          [] -> fail "no msgs") v
  in typeList == Just ["image", "text" :: Text]

-- OpenAI body has the three required top-level keys.
openaiBodyShapeOk :: Bool
openaiBodyShapeOk =
  let v = O.multimodalRequestBody "gpt-4o-test" 1024 ["https://x/y.png"] "hi"
  in case Aeson.parseEither (Aeson.withObject "body" $ \o -> do
                                _ <- o Aeson..: "model"      :: Aeson.Parser Text
                                _ <- o Aeson..: "max_tokens" :: Aeson.Parser Int
                                _ <- o Aeson..: "messages"   :: Aeson.Parser Aeson.Value
                                pure ()) v of
       Right () -> True
       Left _   -> False

-- The image part must be image_url.url == the supplied URL.
openaiBodyImageUrlOk :: Bool
openaiBodyImageUrlOk =
  let url = "https://example.com/x.png"
      v = O.multimodalRequestBody "gpt-4o-test" 1024 [url] "hi"
      gotUrl = Aeson.parseMaybe (Aeson.withObject "body" $ \o -> do
        msgs <- o Aeson..: "messages"
        case (msgs :: [Aeson.Value]) of
          (m:_) -> flip (Aeson.withObject "msg") m $ \mo -> do
            cs <- mo Aeson..: "content"
            case (cs :: [Aeson.Value]) of
              (c:_) -> flip (Aeson.withObject "part") c $ \po -> do
                iu <- po Aeson..: "image_url"
                Aeson.withObject "iu" (\io -> io Aeson..: "url") iu
              [] -> fail "no content"
          [] -> fail "no msgs") v
  in gotUrl == Just url

-- Text part after image part.
openaiBodyOrderOk :: Bool
openaiBodyOrderOk =
  let v = O.multimodalRequestBody "gpt-4o-test" 1024 ["https://x/y.png"] "hi"
      typeList = Aeson.parseMaybe (Aeson.withObject "body" $ \o -> do
        msgs <- o Aeson..: "messages"
        case (msgs :: [Aeson.Value]) of
          (m:_) -> flip (Aeson.withObject "msg") m $ \mo -> do
            cs <- mo Aeson..: "content"
            traverse (Aeson.withObject "part" (\po -> po Aeson..: "type")) (cs :: [Aeson.Value])
          [] -> fail "no msgs") v
  in typeList == Just ["image_url", "text" :: Text]

-- silence "unused" warnings for Aeson.encode / LBS in case we change strategy
_unusedHandle :: LBS.ByteString
_unusedHandle = Aeson.encode (Aeson.Null :: Aeson.Value)

-- =========================================================================
-- Live-API smoke tests, GATED on ANTHROPIC_API_KEY / OPENAI_API_KEY.
-- If the env var is absent the check is reported as a no-op pass so CI
-- never breaks on a missing key.
-- =========================================================================

runLiveSmokeChecks :: IO [(String, Bool)]
runLiveSmokeChecks = do
  anthropicCheck <- runAnthropicLive
  openaiCheck    <- runOpenAILive
  pure [anthropicCheck, openaiCheck]

runAnthropicLive :: IO (String, Bool)
runAnthropicLive = do
  mk <- lookupEnv "ANTHROPIC_API_KEY"
  case mk of
    Nothing -> pure ("anthropic-http live smoke: SKIPPED (ANTHROPIC_API_KEY unset)", True)
    Just k -> do
      mgr <- newManager tlsManagerSettings
      let cfg = A.defaultClaudeConfig (APIKey (T.pack k)) mgr
      ok <- CE.try (runEff
                      . evalState emptyMemoryStore
                      . runMemoryState
                      . runLLMAnthropic cfg
                      $ askText @'AnthropicHttp "Reply with exactly the word: YES")
              :: IO (Either CE.SomeException Text)
      case ok of
        Right t -> pure ("anthropic-http live smoke: round-trip OK (got " <> T.unpack (T.take 60 t) <> ")", "YES" `T.isInfixOf` t)
        Left e  -> pure ("anthropic-http live smoke: FAILED (" <> take 200 (show e) <> ")", False)

runOpenAILive :: IO (String, Bool)
runOpenAILive = do
  mk <- lookupEnv "OPENAI_API_KEY"
  case mk of
    Nothing -> pure ("openai-http live smoke: SKIPPED (OPENAI_API_KEY unset)", True)
    Just k -> do
      mgr <- newManager tlsManagerSettings
      let cfg = O.defaultGPTConfig (APIKey (T.pack k)) mgr
      ok <- CE.try (runEff
                      . evalState emptyMemoryStore
                      . runMemoryState
                      . runLLMOpenAI cfg
                      $ askText @'OpenAIHttp "Reply with exactly the word: YES")
              :: IO (Either CE.SomeException Text)
      case ok of
        Right t -> pure ("openai-http live smoke: round-trip OK (got " <> T.unpack (T.take 60 t) <> ")", "YES" `T.isInfixOf` t)
        Left e  -> pure ("openai-http live smoke: FAILED (" <> take 200 (show e) <> ")", False)

