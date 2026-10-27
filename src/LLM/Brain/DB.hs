{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | beam-postgres persistence for the keyed 'Brain' (and the 'Lexicon'),
-- mirroring ace-ws's beam stack (beam-core + beam-postgres + beam-automigrate).
--
-- The @Brain@\/@Lexicon@ effects are storage-agnostic (see "LLM.Brain.Store" /
-- "LLM.Brain.Lexicon"); this module supplies durable interpreters
-- ('runBrainBeam' \/ 'runLexiconBeam') that swap in unchanged, exactly as
-- 'LLM.Effect.Memory.runMemoryState' could be swapped for a DB backend.
--
-- The intelligence stays in one place: candidate generation and graph walking
-- are SQL, but ranking reuses the pure 'overlapScore' from "LLM.Brain.Store".
-- All columns are @Text@\/@Int64@\/@SqlSerial@, so no custom beam instances are
-- needed; the auto-increment id doubles as the recency clock; and term\/edge
-- entry references are plain @Int64@ (no FK constraints — the interpreter keeps
-- them consistent, and merges needn't fight delete ordering). Schema creation is
-- left entirely to beam-automigrate ('migrateBrainDb').
module LLM.Brain.DB
  ( -- * Schema
    BrainEntryT (..)
  , BrainTermT (..)
  , BrainEdgeT (..)
  , LexiconT (..)
  , BrainDb (..)
  , brainDb
    -- * Migration
  , migrateBrainDb
    -- * Interpreters
  , runBrainBeam
  , runLexiconBeam
    -- * Lexicon seeding (Moby loader + tests)
  , loadMobyPOS
  , posFromMobyCodes
  , upsertLexeme
  , renderPOS
  , parsePOS
  ) where

import Control.Monad (forM_)
import Data.Int (Int64)
import Data.List (sortBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..), comparing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (SqlSerial (..), runInsertReturningList)
import Database.Beam.Postgres (Connection, Pg, Postgres, runBeamPostgres)
import Database.Beam.Migrate (defaultMigratableDbSettings)
import Database.Beam.Migrate.Simple (createSchema)
import Database.Beam.Postgres.Migrate (migrationBackend)

import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)

import LLM.Brain.Key
  ( BrainKey
  , keyTerms
  , parseBrainKey
  , renderBrainKey
  )
import LLM.Brain.Lexicon
  ( Lexicon (..)
  , PartOfSpeech (..)
  , classifyRules
  , lemmatize
  , normalizeWord
  )
import LLM.Brain.Store
  ( Brain (..)
  , BrainEntry (..)
  , BrainId
  , KeyWeights
  , RecallEffort (..)
  , overlapScore
  )

-- Schema ---------------------------------------------------------------------

-- | A keyed entry. The auto-increment 'SqlSerial' id is also the recency clock
-- (larger = newer). The key is stored rendered ('renderBrainKey') and parsed
-- back for scoring.
data BrainEntryT f = BrainEntryT
  { _be_id    :: Columnar f (SqlSerial Int64)
  , _be_key   :: Columnar f Text
  , _be_value :: Columnar f Text
  } deriving (Generic, Beamable)

instance Table BrainEntryT where
  data PrimaryKey BrainEntryT f = BrainEntryKey (Columnar f (SqlSerial Int64))
    deriving (Generic, Beamable)
  primaryKey = BrainEntryKey . _be_id

-- | The inverted index: one row per (entry, term). @_bt_entry@ is the entry's
-- raw id (no FK — kept consistent by the interpreter).
data BrainTermT f = BrainTermT
  { _bt_id    :: Columnar f (SqlSerial Int64)
  , _bt_entry :: Columnar f Int64
  , _bt_term  :: Columnar f Text
  } deriving (Generic, Beamable)

instance Table BrainTermT where
  data PrimaryKey BrainTermT f = BrainTermKey (Columnar f (SqlSerial Int64))
    deriving (Generic, Beamable)
  primaryKey = BrainTermKey . _bt_id

-- | A @[[link]]@ edge between two entries (raw ids; no FK).
data BrainEdgeT f = BrainEdgeT
  { _bedge_id    :: Columnar f (SqlSerial Int64)
  , _bedge_from  :: Columnar f Int64
  , _bedge_label :: Columnar f Text
  , _bedge_to    :: Columnar f Int64
  } deriving (Generic, Beamable)

instance Table BrainEdgeT where
  data PrimaryKey BrainEdgeT f = BrainEdgeKey (Columnar f (SqlSerial Int64))
    deriving (Generic, Beamable)
  primaryKey = BrainEdgeKey . _bedge_id

-- | The POS dictionary (e.g. Moby): word → part of speech (stored as text).
data LexiconT f = LexiconT
  { _lex_word :: Columnar f Text
  , _lex_pos  :: Columnar f Text
  } deriving (Generic, Beamable)

instance Table LexiconT where
  data PrimaryKey LexiconT f = LexiconKey (Columnar f Text)
    deriving (Generic, Beamable)
  primaryKey = LexiconKey . _lex_word

data BrainDb f = BrainDb
  { _bdb_entries :: f (TableEntity BrainEntryT)
  , _bdb_terms   :: f (TableEntity BrainTermT)
  , _bdb_edges   :: f (TableEntity BrainEdgeT)
  , _bdb_lexicon :: f (TableEntity LexiconT)
  } deriving (Generic, Database be)

brainDb :: DatabaseSettings be BrainDb
brainDb = defaultDbSettings

-- | Create the brain tables. The schema is derived generically from the Haskell
-- types by beam-migrate ('defaultMigratableDbSettings'); 'createSchema' then
-- issues the @CREATE TABLE@ statements. For initial setup against a fresh
-- database — createSchema emits plain @CREATE TABLE@ (no @IF NOT EXISTS@), so to
-- evolve an existing schema use beam-migrate's @bringUpToDate@.
--
-- (beam-automigrate would be the richer choice, with diffing, but doesn't
-- compile on GHC 9.10: it relies on Control.Monad re-exports mtl 2.3 removed.)
migrateBrainDb :: Connection -> IO ()
migrateBrainDb conn =
  runBeamPostgres conn (createSchema migrationBackend (defaultMigratableDbSettings @Postgres @BrainDb))

-- PartOfSpeech <-> text -------------------------------------------------------

renderPOS :: PartOfSpeech -> Text
renderPOS = \case
  Noun -> "Noun"; Verb -> "Verb"; Both -> "Both"; Adjective -> "Adjective"; OtherNoise -> "OtherNoise"

parsePOS :: Text -> Maybe PartOfSpeech
parsePOS = \case
  "Noun" -> Just Noun; "Verb" -> Just Verb; "Both" -> Just Both
  "Adjective" -> Just Adjective; "OtherNoise" -> Just OtherNoise; _ -> Nothing

-- Lexicon interpreter --------------------------------------------------------

-- | Classify via the DB POS table, falling back to the rule classifier for
-- out-of-vocabulary words; lemmatise via the Porter rules (a POS dictionary
-- carries no lemmas).
runLexiconBeam :: (IOE :> es) => Connection -> Eff (Lexicon : es) a -> Eff es a
runLexiconBeam conn = interpret $ \_ -> \case
  Classify w -> liftIO $ do
    let nw = normalizeWord w
    hit <- runBeamPostgres conn $ runSelectReturningOne $ select $ do
             l <- all_ (_bdb_lexicon brainDb)
             guard_ (_lex_word l ==. val_ nw)
             pure (_lex_pos l)
    pure $ case hit >>= parsePOS of
      Just pos -> pos
      Nothing  -> classifyRules w
  Lemma w -> pure (lemmatize w)

-- | Insert or update a single lexicon row (POS dictionary seed).
upsertLexeme :: Connection -> Text -> PartOfSpeech -> IO ()
upsertLexeme conn word pos = runBeamPostgres conn $ do
  runDelete $ delete (_bdb_lexicon brainDb) (\l -> _lex_word l ==. val_ word)
  runInsert $ insert (_bdb_lexicon brainDb) $ insertExpressions
    [ LexiconT (val_ word) (val_ (renderPOS pos)) ]

-- | Map a Moby part-of-speech code string to our 'PartOfSpeech'. Moby codes:
-- @N@ noun, @p@ noun-plural, @h@ noun-phrase, @o@ nominative; @V@ verb
-- (participle), @t@ transitive, @i@ intransitive; @A@ adjective; @v@ adverb;
-- plus @C@\/@P@\/@!@\/@r@\/@D@\/@I@ function words. Noun + verb senses combine to
-- 'Both'; a noun sense wins over a bare adjective sense; an adverb-\/function-only
-- word is 'OtherNoise' (correctly dropped from keys).
posFromMobyCodes :: String -> PartOfSpeech
posFromMobyCodes codes
  | hasNoun && hasVerb = Both
  | hasNoun            = Noun
  | hasVerb            = Verb
  | hasAdj             = Adjective
  | otherwise          = OtherNoise
  where
    hasNoun = any (`elem` codes) ("Npho" :: String)
    hasVerb = any (`elem` codes) ("Vti" :: String)
    hasAdj  = 'A' `elem` codes

-- | Bulk-load a Moby Part-of-Speech TSV (@word<TAB>codes@ per line) into the
-- lexicon table, replacing its contents. Returns the number of rows loaded.
-- The data is the public-domain Moby POS list (Grady Ward), pre-filtered to
-- single lowercase-alphabetic words; the raw Moby codes are interpreted by
-- 'posFromMobyCodes' here.
loadMobyPOS :: Connection -> FilePath -> IO Int
loadMobyPOS conn path = do
  contents <- TIO.readFile path
  let rows =
        [ (w, renderPOS (posFromMobyCodes (T.unpack (T.drop 1 rest))))
        | line <- T.lines contents
        , let (w, rest) = T.breakOn "\t" line
        , not (T.null w)
        , not (T.null rest)
        ]
  runBeamPostgres conn $ do
    runDelete $ delete (_bdb_lexicon brainDb) (\_ -> val_ True)
    forM_ (chunksOf 2000 rows) $ \chunk ->
      runInsert $ insert (_bdb_lexicon brainDb) $
        insertExpressions [ LexiconT (val_ w) (val_ p) | (w, p) <- chunk ]
  pure (length rows)

-- | Split a list into chunks of at most @n@ (for batched inserts).
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (a, b) = splitAt n xs in a : chunksOf n b

-- Brain interpreter ----------------------------------------------------------

-- | Durable brain over beam-postgres. Closes over the (globally-decided)
-- 'KeyWeights' and a connection.
runBrainBeam :: (IOE :> es) => KeyWeights -> Connection -> Eff (Brain : es) a -> Eff es a
runBrainBeam weights conn = interpret $ \_ -> \case
  RememberKeyed key value -> liftIO (beamRemember conn key value)
  GetEntry i              -> liftIO (beamGetEntry conn i)
  LinkEntries from label to -> liftIO $ runBeamPostgres conn $ runInsert $
    insert (_bdb_edges brainDb) $ insertExpressions
      [ BrainEdgeT default_ (val_ (fromIntegral from)) (val_ label) (val_ (fromIntegral to)) ]
  EntryNeighbors i -> liftIO $ do
    rows <- runBeamPostgres conn $ runSelectReturningList $ select $ do
      e <- all_ (_bdb_edges brainDb)
      guard_ (_bedge_from e ==. val_ (fromIntegral i))
      pure (_bedge_label e, _bedge_to e)
    pure [(label, fromIntegral to) | (label, to) <- rows]
  AllEntries -> liftIO $ do
    rows <- runBeamPostgres conn $ runSelectReturningList $ select $ all_ (_bdb_entries brainDb)
    pure (mapMaybe toEntry rows)
  RecallByKey qkey effort -> liftIO (beamRecall weights conn qkey effort)
  Collisions -> liftIO $ do
    rows <- runBeamPostgres conn $ runSelectReturningList $ select $ do
      e <- all_ (_bdb_entries brainDb)
      pure (_be_key e, _be_id e)
    let grouped = Map.fromListWith (++) [ (k, [serialToId i]) | (k, i) <- rows ]
    pure [ (k, sortBy compare ids) | (k, ids) <- Map.toList grouped, length ids > 1 ]
  ReplaceEntries ids newKey newValue -> liftIO (beamReplace conn ids newKey newValue)

-- | A loaded entry row → 'BrainEntry' (recency = the id). 'Nothing' if the
-- stored key text is unparseable.
toEntry :: BrainEntryT Identity -> Maybe BrainEntry
toEntry row = do
  key <- parseBrainKey (_be_key row)
  let i = serialToId (_be_id row)
  pure (BrainEntry i key (_be_value row) (fromIntegral i))

serialToId :: SqlSerial Int64 -> BrainId
serialToId (SqlSerial n) = fromIntegral n

beamRemember :: Connection -> BrainKey -> Text -> IO BrainId
beamRemember conn key value = runBeamPostgres conn $ do
  inserted <- runInsertReturningList $ insert (_bdb_entries brainDb) $ insertExpressions
    [ BrainEntryT default_ (val_ (renderBrainKey key)) (val_ value) ]
  case inserted of
    []        -> pure (-1)
    (row : _) -> do
      let i = serialToId (_be_id row)
      insertTerms i (keyTerms key)
      pure i

-- | Register an entry's terms in the inverted index.
insertTerms :: BrainId -> [Text] -> Pg ()
insertTerms _ [] = pure ()
insertTerms i terms = runInsert $ insert (_bdb_terms brainDb) $ insertExpressions
  [ BrainTermT default_ (val_ (fromIntegral i)) (val_ t) | t <- terms ]

beamGetEntry :: Connection -> BrainId -> IO (Maybe BrainEntry)
beamGetEntry conn i = do
  row <- runBeamPostgres conn $ runSelectReturningOne $ select $ do
    e <- all_ (_bdb_entries brainDb)
    guard_ (_be_id e ==. val_ (SqlSerial (fromIntegral i)))
    pure e
  pure (row >>= toEntry)

-- | The deterministic recall: SQL candidate-gen by shared terms, pure overlap
-- scoring, then SQL breadth-first spreading along edges.
beamRecall :: KeyWeights -> Connection -> BrainKey -> RecallEffort -> IO [BrainEntry]
beamRecall weights conn qkey effort = do
  let terms = keyTerms qkey
  if null terms
    then pure []
    else do
      candIds <- runBeamPostgres conn $ runSelectReturningList $ select $ nub_ $ do
        t <- all_ (_bdb_terms brainDb)
        guard_ (_bt_term t `in_` map val_ terms)
        pure (_bt_entry t)
      cands <- loadEntries conn (map fromIntegral candIds)
      let scored = [ (overlapScore weights qkey (beKey e), e) | e <- cands ]
          seeds = take (reMaxSeeds effort)
                . sortBy (comparing (\(s, e) -> (Down s, Down (beTime e))))
                $ filter ((>= reThreshold effort) . fst) scored
          seedIds = map (beId . snd) seeds
      spreadIds <- bfsSpread conn seedIds (reHops effort)
      let seedSet = Set.fromList seedIds
      spreadEntries <- loadEntries conn (filter (`Set.notMember` seedSet) spreadIds)
      let orderedSpread = sortBy (comparing (Down . beTime)) spreadEntries
      pure (map snd seeds ++ orderedSpread)

-- | Load entries by id (empty list → no query, avoiding @IN ()@).
loadEntries :: Connection -> [BrainId] -> IO [BrainEntry]
loadEntries _ [] = pure []
loadEntries conn ids = do
  rows <- runBeamPostgres conn $ runSelectReturningList $ select $ do
    e <- all_ (_bdb_entries brainDb)
    guard_ (_be_id e `in_` map (val_ . SqlSerial . fromIntegral) ids)
    pure e
  pure (mapMaybe toEntry rows)

-- | The set of entry ids reachable within @hops@ steps of the seeds (seeds
-- included), walking edges level by level via SQL.
bfsSpread :: Connection -> [BrainId] -> Int -> IO [BrainId]
bfsSpread _ seeds hops | null seeds || hops <= 0 = pure seeds
bfsSpread conn seeds hops = go (Set.fromList seeds) seeds hops
  where
    go visited _ 0 = pure (Set.toList visited)
    go visited frontier n
      | null frontier = pure (Set.toList visited)
      | otherwise = do
          tos <- runBeamPostgres conn $ runSelectReturningList $ select $ nub_ $ do
            e <- all_ (_bdb_edges brainDb)
            guard_ (_bedge_from e `in_` map (val_ . fromIntegral) frontier)
            pure (_bedge_to e)
          let next = filter (`Set.notMember` visited) (map fromIntegral tos)
          go (foldr Set.insert visited next) next (n - 1)

-- | Merge: insert the fused entry, rewire external edges onto it, drop the old
-- entries\/terms\/internal edges. Runs in one transaction.
beamReplace :: Connection -> [BrainId] -> BrainKey -> Text -> IO BrainId
beamReplace conn ids newKey newValue = runBeamPostgres conn $ do
  let idSet = Set.fromList ids
      ids64 = map fromIntegral ids :: [Int64]
  -- external edges (touch the group on exactly one side), captured before deletes
  edgeRows <- runSelectReturningList $ select $ do
    e <- all_ (_bdb_edges brainDb)
    guard_ (_bedge_from e `in_` map val_ ids64 ||. _bedge_to e `in_` map val_ ids64)
    pure (_bedge_from e, _bedge_label e, _bedge_to e)
  inserted <- runInsertReturningList $ insert (_bdb_entries brainDb) $ insertExpressions
    [ BrainEntryT default_ (val_ (renderBrainKey newKey)) (val_ newValue) ]
  case inserted of
    []        -> pure (-1)
    (row : _) -> do
      let newId = serialToId (_be_id row)
          newId64 = fromIntegral newId :: Int64
          remap x = if Set.member (fromIntegral x) idSet then newId64 else x
          external =
            [ (remap f, lbl, remap t)
            | (f, lbl, t) <- edgeRows
            , not (Set.member (fromIntegral f) idSet && Set.member (fromIntegral t) idSet)
            ]
      insertTerms newId (keyTerms newKey)
      runDelete $ delete (_bdb_edges brainDb)
        (\e -> _bedge_from e `in_` map val_ ids64 ||. _bedge_to e `in_` map val_ ids64)
      mapM_ (\(f, lbl, t) -> runInsert $ insert (_bdb_edges brainDb) $ insertExpressions
               [ BrainEdgeT default_ (val_ f) (val_ lbl) (val_ t) ]) external
      runDelete $ delete (_bdb_terms brainDb) (\t -> _bt_entry t `in_` map val_ ids64)
      runDelete $ delete (_bdb_entries brainDb)
        (\e -> _be_id e `in_` map (val_ . SqlSerial) ids64)
      pure newId
