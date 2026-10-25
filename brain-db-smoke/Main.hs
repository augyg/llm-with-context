{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | A runtime smoke test for the beam-postgres brain interpreters
-- ("LLM.Brain.DB"). It is compile-checked by nix but NOT run by @doCheck@ (the
-- nix sandbox has no Postgres). Run it by hand against an ephemeral database:
--
-- > nix-shell -p postgresql --run '
-- >   initdb -D /tmp/pg; pg_ctl -D /tmp/pg -o "-k /tmp -h \"\"" -l /tmp/pg.log start
-- >   createdb -h /tmp brain_smoke
-- >   result/bin/brain-db-smoke "host=/tmp dbname=brain_smoke"'
--
-- It migrates the schema, then asserts: recall ranks a perfect noun match above
-- a partial one and reaches a linked-but-disjoint entry via spreading
-- activation; the lexicon returns a DB-stored POS (one that differs from the
-- rule classifier, proving the lookup) and falls back to the rules for an
-- out-of-vocabulary word.
module Main (main) where

import Control.Monad (forM_, unless)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (listToMaybe)
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Effectful (runEff)
import Database.PostgreSQL.Simple (connectPostgreSQL)

import LLM.Brain.Key (canonicalKey)
import LLM.Brain.Lexicon (PartOfSpeech (..), classify)
import LLM.Brain.Store
  ( beValue
  , defaultEffort
  , defaultWeights
  , linkEntries
  , recallByKey
  , rememberKeyed
  )
import LLM.Brain.DB (loadMobyPOS, migrateBrainDb, runBrainBeam, runLexiconBeam, upsertLexeme)

main :: IO ()
main = do
  args <- getArgs
  let (conninfo, mMoby) = case args of
        (c : rest) -> (BS.pack c, listToMaybe rest)
        []         -> ("host=/tmp dbname=brain_smoke", Nothing)
  conn <- connectPostgreSQL conninfo
  migrateBrainDb conn

  -- Seed two lexicon rows. "telemetry" -> Noun in the DB differs from the rule
  -- classifier (which has no suffix rule for it and would answer Both), so a
  -- Noun result proves the DB lookup actually fired.
  upsertLexeme conn "telemetry" Noun
  upsertLexeme conn "scrape" Both

  recalled <- runEff . runBrainBeam defaultWeights conn $ do
    a <- rememberKeyed (canonicalKey ["scrape", "bot"] [] []) "A"
    _ <- rememberKeyed (canonicalKey ["effect", "memory"] [] []) "B"
    _ <- rememberKeyed (canonicalKey ["scrape", "proxy"] [] []) "C"
    d <- rememberKeyed (canonicalKey ["zzz"] [] []) "D"
    linkEntries a "see" d
    map beValue <$> recallByKey (canonicalKey ["scrape", "bot"] [] []) defaultEffort

  (posTelemetry, posUnknown) <- runEff . runLexiconBeam conn $ do
    t <- classify "telemetry"
    u <- classify "xqzwv"
    pure (t, u)

  let baseChecks =
        [ ( "brain-db: recall ranks A (perfect), C (partial), spreads to linked D"
          , recalled == ["A", "C", "D"]
          )
        , ("brain-db: lexicon DB hit returns stored POS (Noun, not rule's Both)", posTelemetry == Noun)
        , ("brain-db: lexicon OOV word falls back to the rule classifier", posUnknown == Both)
        ]

  -- If a Moby-POS TSV path is given, load it and verify real-dictionary
  -- lookups. computer/quickly differ from what the rule classifier answers
  -- (Both), so a correct result proves the DB POS is in force.
  mobyChecks <- case mMoby of
    Nothing -> pure []
    Just mobyPath -> do
      loaded <- loadMobyPOS conn mobyPath
      (pComputer, pQuickly, pBeautiful) <- runEff . runLexiconBeam conn $ do
        c <- classify "computer"
        q <- classify "quickly"
        b <- classify "beautiful"
        pure (c, q, b)
      pure
        [ ("brain-db: Moby load populated the lexicon (>150k rows)", loaded > 150000)
        , ("brain-db: Moby 'computer' -> Noun (DB, not rule's Both)", pComputer == Noun)
        , ("brain-db: Moby 'quickly' -> OtherNoise (adverb, not rule's Both)", pQuickly == OtherNoise)
        , ("brain-db: Moby 'beautiful' -> Adjective", pBeautiful == Adjective)
        ]

  let checks = baseChecks ++ mobyChecks
  forM_ checks $ \(name, ok) ->
    putStrLn $ (if ok then "PASS  " else "FAIL  ") <> name
  unless (all snd checks) exitFailure
