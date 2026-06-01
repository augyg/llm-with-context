{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Pure tests for the effect layer. Everything here runs via 'runPureEff' —
-- no network, no IO — by interpreting the LLM effect with the mock backend and
-- Memory/Tool over their pure interpreters. Dependency-light on purpose (just
-- base/text/effectful-core/the lib): a hand-rolled harness, no test framework.
module Main (main) where

import Control.Monad (forM_, unless)
import Data.Text (Text)
import System.Exit (exitFailure)

import Effectful (runPureEff)
import Effectful.State.Static.Local (evalState)

import LLM.Effect (askWithContext)
import LLM.Effect.Memory
  ( emptyMemoryStore
  , pin
  , prune
  , recallAll
  , remember
  , runMemoryState
  )
import LLM.Effect.Mock (runLLMMock)
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
  , ConvoAnswer (..)
  , ConvoQuery (..)
  , ConvoQuestion (..)
  , GPTRole (User)
  , RelevantContext (..)
  , Tag (..)
  , cwr
  )

import LLM.Brain.Lexicon (PartOfSpeech (..), classifyRules, lemmatize, runLexiconRules)
import LLM.Brain.Key (BrainKey (..), canonicalKey, keyOf)
import LLM.Brain.Store
  ( BrainEntry (..)
  , allEntries
  , collisions
  , compactConcat
  , defaultEffort
  , defaultWeights
  , emptyBrain
  , getEntry
  , linkEntries
  , recallByKey
  , rememberKeyed
  , runBrainState
  )
import LLM.Brain.Catalogue (catalogue, keyForPrompt, recallFocused)

main :: IO ()
main = do
  let checks =
        [ ("memory: remember/recallAll round-trip", memCount == 2)
        , ("memory: pinned turn survives prune", pinnedTags == ["keep"])
        , ("effect: context loop records each turn", ctxTurns == 2)
        , ("tool: runCommand with pure handlers", toolStdout == "ls")
        , ("sandbox: jail accepts relative, rejects absolute + ..", jailOk)
        , ("sandbox: shellCommandPaths extracts the path", shellCommandPaths (Grep "x" "sub/dir" False False) == ["sub/dir"])
        , ("sandbox: bwrap argv binds root + ro-binds store + runs exe", bwrapArgvOk)
        , ("sandbox: sandbox-exec profile confines writes to root", sbProfileOk)
        , ("brain/lexicon: inflections collapse to one lemma", lemmaConsistency)
        , ("brain/lexicon: POS buckets (noise / noun / adjective)", posBuckets)
        , ("brain/key: keyOf buckets nouns vs verbs, drops noise", keyBucketsOk)
        , ("brain/recall: ranks by weighted overlap, excludes non-matches", recallRankingOk)
        , ("brain/recall: spreading activation reaches a linked entry", spreadRecallOk)
        , ("brain/compact: identical-key entries merge (Concat)", collisionMergeOk)
        , ("brain/catalogue: mock write path keys via the Lexicon", catalogueKeyOk)
        , ("brain/focused: keyForPrompt extracts a focused key via the LLM", keyForPromptOk)
        , ("brain/focused: recallFocused recalls the keyed entry", recallFocusedOk)
        ]
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
    . runLLMMock @'Anthropic (\_ -> Right "ok")
    $ do
        _ <- askWithContext @'Anthropic (LastN 10) (Tag "q1", ConvoQuestion [cwr User "hi"])
        _ <- askWithContext @'Anthropic (LastN 10) (Tag "q2", ConvoQuestion [cwr User "again"])
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

-- Brain: keyed-memory system -------------------------------------------------

-- Inflections must collapse to the same lemma (this is what lets a written
-- "scrape" match a read "scraping"), while doubling vs e-restoration is kept
-- distinct (hopping/hoping).
lemmaConsistency :: Bool
lemmaConsistency =
  lemmatize "scraping" == lemmatize "scrape"
    && lemmatize "running" == "run"
    && lemmatize "studies" == lemmatize "study"
    && lemmatize "queries" == "query"
    && lemmatize "hopping" /= lemmatize "hoping"

-- Closed-class words are noise; suffix morphology recovers noun/adjective.
posBuckets :: Bool
posBuckets =
  classifyRules "the" == OtherNoise
    && classifyRules "happiness" == Noun
    && classifyRules "beautiful" == Adjective

-- keyOf splits a sentence into noun/verb buckets and drops function words.
keyBuckets :: BrainKey
keyBuckets = runPureEff . runLexiconRules $ keyOf "barking dogs chase the cat"

keyBucketsOk :: Bool
keyBucketsOk =
  "bark" `elem` bkVerbs keyBuckets
    && "dog" `elem` bkNouns keyBuckets
    && "cat" `elem` bkNouns keyBuckets
    && "the" `notElem` (bkNouns keyBuckets ++ bkVerbs keyBuckets)

-- Recall ranks by weighted overlap: a perfect noun match (A) beats a partial
-- one (C); a non-overlapping entry (B) is not a candidate at all.
recallRanking :: [Text]
recallRanking = runPureEff . evalState emptyBrain . runBrainState defaultWeights $ do
  _ <- rememberKeyed (canonicalKey ["scrape", "bot"] [] []) "A"
  _ <- rememberKeyed (canonicalKey ["effect", "memory"] [] []) "B"
  _ <- rememberKeyed (canonicalKey ["scrape", "proxy"] [] []) "C"
  map beValue <$> recallByKey (canonicalKey ["scrape", "bot"] [] []) defaultEffort

recallRankingOk :: Bool
recallRankingOk = recallRanking == ["A", "C"]

-- Spreading activation: a linked entry is recalled even with a disjoint key.
spreadRecall :: [Text]
spreadRecall = runPureEff . evalState emptyBrain . runBrainState defaultWeights $ do
  a <- rememberKeyed (canonicalKey ["alpha"] [] []) "A"
  b <- rememberKeyed (canonicalKey ["zzz"] [] []) "B"
  linkEntries a "see" b
  map beValue <$> recallByKey (canonicalKey ["alpha"] [] []) defaultEffort

spreadRecallOk :: Bool
spreadRecallOk = spreadRecall == ["A", "B"]

-- Two entries with an identical canonical key are one collision group, and
-- compactConcat fuses their values into a single entry.
collisionMerge :: (Int, Int, [Text])
collisionMerge = runPureEff . evalState emptyBrain . runBrainState defaultWeights $ do
  _ <- rememberKeyed (canonicalKey ["dog"] [] []) "v1"
  _ <- rememberKeyed (canonicalKey ["dog"] [] []) "v2"
  before <- length <$> collisions
  merged <- compactConcat
  vals <- map beValue <$> allEntries
  pure (before, merged, vals)

collisionMergeOk :: Bool
collisionMergeOk = collisionMerge == (1, 1, ["v1\n\nv2"])

-- Catalogue (write path) via the mock LLM: the model's raw terms are lemmatised
-- through the same Lexicon the read path uses and stored as a canonical key
-- (scrapers -> scraper, bots -> bot).
catalogueKey :: Maybe BrainKey
catalogueKey =
  runPureEff
    . evalState emptyMemoryStore
    . runMemoryState
    . evalState emptyBrain
    . runBrainState defaultWeights
    . runLexiconRules
    . runLLMMock @'Anthropic (const (Right cannedJson))
    $ do
        res <- catalogue @'Anthropic "the scrapers detect bots"
        case res of
          Left _  -> pure Nothing
          Right i -> fmap (fmap beKey) (getEntry i)
  where
    cannedJson =
      "{\"nouns\":[\"scrapers\",\"bots\"],\"verbs\":[\"detect\"],\"adjectives\":[],\"value\":\"## note\"}"

catalogueKeyOk :: Bool
catalogueKeyOk = catalogueKey == Just (canonicalKey ["scraper", "bot"] ["detect"] [])

-- Focused read path: an LLM pre-call (mocked) extracts just the salient
-- keyword + POS for "tell me about butterflies", canonicalised through the same
-- Lexicon as everything else.
focusedKeyJson :: Text
focusedKeyJson = "{\"nouns\":[\"butterflies\"],\"verbs\":[],\"adjectives\":[]}"

keyForPromptKey :: Maybe BrainKey
keyForPromptKey =
  runPureEff
    . evalState emptyMemoryStore
    . runMemoryState
    . runLexiconRules
    . runLLMMock @'Anthropic (const (Right focusedKeyJson))
    $ do
        e <- keyForPrompt @'Anthropic "tell me about butterflies"
        pure (either (const Nothing) Just e)

keyForPromptOk :: Bool
keyForPromptOk = keyForPromptKey == Just (canonicalKey [lemmatize "butterflies"] [] [])

-- recallFocused: the focused key then recalls the entry stored under it.
recallFocusedVals :: [Text]
recallFocusedVals =
  runPureEff
    . evalState emptyMemoryStore
    . runMemoryState
    . evalState emptyBrain
    . runBrainState defaultWeights
    . runLexiconRules
    . runLLMMock @'Anthropic (const (Right focusedKeyJson))
    $ do
        _ <- rememberKeyed (canonicalKey [lemmatize "butterflies"] [] []) "BUTTERFLY-NOTE"
        e <- recallFocused @'Anthropic defaultEffort "tell me about butterflies"
        pure (either (const []) (map beValue) e)

recallFocusedOk :: Bool
recallFocusedOk = recallFocusedVals == ["BUTTERFLY-NOTE"]
