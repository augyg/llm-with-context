{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | @Lexicon@ — the deterministic lexical layer the keyed brain rests on.
--
-- It answers two questions about a single word, with NO LLM and NO statistical
-- model, so read-time keying ("LLM.Brain.Key") costs nothing per query:
--
--   * 'classify' — its 'PartOfSpeech' (noun \/ verb \/ both \/ adjective \/ noise);
--   * 'lemma'    — its canonical base form, so inflections collapse together.
--
-- 'runLexiconRules' is the pure, data-file-free default backend: a closed-class
-- stop list + a small seed table of irregular\/ambiguous words + deterministic
-- morphology (a Porter-stemmer core for inflections, an irregulars table for the
-- forms Porter can't reach). Following the 'LLM.Effect.Memory' pattern, the
-- backing data is swappable by swapping the interpreter — e.g. a Moby-POS table
-- in beam-postgres — without touching any caller. The pure functions
-- ('classifyRules' \/ 'lemmatize') are exported so a DB-backed interpreter can
-- reuse them as its out-of-vocabulary fallback, and so tests can call them
-- directly.
module LLM.Brain.Lexicon
  ( -- * Part of speech
    PartOfSpeech (..)
    -- * Effect
  , Lexicon (..)
  , classify
  , lemma
    -- * Default (pure, rule-based) interpreter
  , runLexiconRules
    -- * Pure layer (reused by DB interpreters as the OOV fallback, and by tests)
  , classifyRules
  , lemmatize
  , normalizeWord
  , isStopWord
  , porterStem
  ) where

import Data.Char (isAlpha)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (interpret, send)

-- | The part of speech a single word maps to. 'Both' = the word is commonly
-- either a noun or a verb (e.g. @run@, @scrape@, @commit@) — on the read path it
-- is bucketed into both noun and verb sets so it can match either. 'OtherNoise'
-- = a closed-class \/ function word that carries no indexing value (articles,
-- prepositions, pronouns, auxiliaries, …) and is dropped from keys.
data PartOfSpeech = Noun | Verb | Both | Adjective | OtherNoise
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The deterministic lexical effect (dynamic dispatch). Two pure queries about
-- one word; the backing dictionary is the interpreter's concern.
data Lexicon :: Effect where
  Classify :: T.Text -> Lexicon m PartOfSpeech
  Lemma    :: T.Text -> Lexicon m T.Text

type instance DispatchOf Lexicon = Dynamic

-- | The 'PartOfSpeech' of a word.
classify :: (Lexicon :> es) => T.Text -> Eff es PartOfSpeech
classify = send . Classify

-- | The canonical base form of a word (so inflections collapse to one key term).
lemma :: (Lexicon :> es) => T.Text -> Eff es T.Text
lemma = send . Lemma

-- | The default backend: pure, deterministic, no data file. Also the natural
-- out-of-vocabulary fallback for a dictionary-backed interpreter.
runLexiconRules :: Eff (Lexicon : es) a -> Eff es a
runLexiconRules = interpret $ \_ -> \case
  Classify w -> pure (classifyRules w)
  Lemma w    -> pure (lemmatize w)

-- Classification ------------------------------------------------------------

-- | Deterministic part-of-speech classification: stop words and 1-letter tokens
-- are 'OtherNoise'; a seed table pins common irregular \/ ambiguous words;
-- everything else is decided by suffix morphology, defaulting to 'Both' (an
-- unknown content word might be a noun or a verb, so allow it to match either —
-- the weighted overlap in "LLM.Brain.Store" tolerates the extra bucket).
classifyRules :: T.Text -> PartOfSpeech
classifyRules raw =
  let w = normalizeWord raw
  in if T.length w <= 1
       then OtherNoise
       else if isStopWord w
              then OtherNoise
              else case Map.lookup w seedTable of
                     Just pos -> pos
                     Nothing  -> bySuffix w

-- | Suffix-based fallback classification. Adjective and noun suffixes are
-- checked before verb suffixes (the @-ing@\/@-ed@ verb tests are broad). An
-- unrecognised content word is 'Both'.
bySuffix :: T.Text -> PartOfSpeech
bySuffix w
  | anySuffix ["ous", "ful", "ive", "able", "ible", "ish", "less", "like", "ical"] = Adjective
  | anySuffix ["tion", "sion", "ment", "ness", "ity", "ance", "ence", "ship", "hood", "ism", "ist", "ology"] = Noun
  | anySuffix ["ize", "ise", "ify", "ate"] = Verb
  | anySuffix ["ing", "ed"] = Verb
  | otherwise = Both
  where
    anySuffix = any (`T.isSuffixOf` w)

-- Lemmatization -------------------------------------------------------------

-- | Reduce a word to its canonical base form: irregular verbs \/ plurals via a
-- table (@went -> go@, @children -> child@), everything else via the Porter
-- stemmer's inflectional core. Idempotent on already-base forms, and crucially
-- /consistent/: an inflected form and its base reduce to the same token, so they
-- match in a key.
lemmatize :: T.Text -> T.Text
lemmatize raw =
  let w = normalizeWord raw
  in case Map.lookup w irregulars of
       Just base -> base
       Nothing   -> porterStem w

-- | Lowercase, then keep only alphabetic characters.
normalizeWord :: T.Text -> T.Text
normalizeWord = T.filter isAlpha . T.toLower

-- | Closed-class \/ function words that carry no indexing value.
isStopWord :: T.Text -> Bool
isStopWord w = Set.member w stopWords

-- Data tables ---------------------------------------------------------------

stopWords :: Set.Set T.Text
stopWords = Set.fromList
  [ "the","a","an","and","or","but","if","then","else","when","while","of","to"
  , "in","on","at","by","for","with","about","against","between","into","through"
  , "during","before","after","above","below","from","up","down","out","off","over"
  , "under","again","further","once","here","there","all","any","both","each","few"
  , "more","most","other","some","such","no","nor","not","only","own","same","so"
  , "than","too","very","can","will","just","should","now","is","am","are","was"
  , "were","be","been","being","have","has","had","having","do","does","did","doing"
  , "would","could","shall","may","might","must","i","you","he","she","it","we","they"
  , "me","him","her","us","them","my","your","his","its","our","their","mine","yours"
  , "this","that","these","those","who","whom","which","what","whose","as","because"
  , "until","upto","per","via","yes","ok","okay","also","etc","im","ive","dont","cant"
  , "thats","gonna","wanna","gotta","theyre","youre","whats","heres","theres"
  ]

-- | Common words where suffix rules fail or that are high-value to pin. @Both@
-- marks noun-or-verb words; the rest correct mis-firing suffix heuristics.
seedTable :: Map.Map T.Text PartOfSpeech
seedTable = Map.fromList
  [ ("run", Both), ("scrape", Both), ("code", Both), ("test", Both), ("build", Both)
  , ("fix", Both), ("commit", Both), ("type", Both), ("bug", Both), ("link", Both)
  , ("merge", Both), ("parse", Both), ("query", Both), ("store", Both), ("cache", Both)
  , ("name", Both), ("key", Both), ("value", Noun), ("token", Noun), ("model", Noun)
  , ("effect", Noun), ("memory", Noun), ("context", Noun), ("history", Noun)
  , ("data", Noun), ("table", Noun), ("graph", Noun), ("node", Noun), ("edge", Noun)
  , ("config", Noun), ("error", Noun), ("library", Noun), ("module", Noun)
  , ("function", Noun), ("system", Noun), ("recall", Both), ("remember", Verb)
  , ("ask", Verb), ("classify", Verb), ("page", Noun), ("request", Both)
  -- words whose suffix would mislead
  , ("the", OtherNoise), ("being", OtherNoise), ("during", OtherNoise)
  , ("interesting", Adjective), ("missing", Adjective), ("data", Noun)
  ]

-- | Irregular verb \/ plural base forms the Porter core does not reach.
irregulars :: Map.Map T.Text T.Text
irregulars = Map.fromList
  [ ("went","go"),("gone","go"),("goes","go"),("going","go")
  , ("ran","run"),("running","run")
  , ("was","be"),("were","be"),("been","be"),("being","be"),("is","be"),("are","be"),("am","be")
  , ("had","have"),("has","have"),("having","have")
  , ("did","do"),("done","do"),("does","do"),("doing","do")
  , ("made","make"),("making","make"),("makes","make")
  , ("said","say"),("saying","say")
  , ("took","take"),("taken","take"),("taking","take")
  , ("came","come"),("coming","come")
  , ("got","get"),("gotten","get"),("getting","get")
  , ("wrote","write"),("written","write"),("writing","write")
  , ("built","build"),("building","build")
  , ("found","find"),("finding","find")
  , ("thought","think"),("thinking","think")
  , ("bought","buy"),("brought","bring"),("caught","catch"),("taught","teach")
  , ("children","child"),("men","man"),("women","woman"),("people","person")
  , ("feet","foot"),("teeth","tooth"),("mice","mouse"),("geese","goose")
  , ("queries","query"),("indices","index"),("vertices","vertex"),("matrices","matrix")
  ]

-- Porter stemmer (inflectional core: steps 1a, 1b, 1c) -----------------------

-- | The Porter-stemmer inflectional core, on lowercase alphabetic text. Folds
-- plurals (@-sses@\/@-ies@\/@-s@), past\/progressive (@-ed@\/@-ing@ with proper
-- @e@-restoration and de-doubling), and final @y -> i@. Words of length ≤ 2 are
-- returned unchanged. (Porter steps 2–5 — derivational suffixes — are
-- intentionally omitted; a dictionary-backed interpreter covers those.)
porterStem :: T.Text -> T.Text
porterStem t
  | T.length t <= 2 = t
  | otherwise       = T.pack . step1c . step1b . step1a . T.unpack $ t

step1a :: String -> String
step1a w
  | "sses" `isSuf` w = dropEnd 2 w
  | "ies"  `isSuf` w = dropEnd 2 w
  | "ss"   `isSuf` w = w
  | "s"    `isSuf` w = dropEnd 1 w
  | otherwise        = w

step1b :: String -> String
step1b w
  | "eed" `isSuf` w = if measure (dropEnd 3 w) > 0 then dropEnd 1 w else w
  | "ed"  `isSuf` w = let stem = dropEnd 2 w in if containsVowel stem then post1b stem else w
  | "ing" `isSuf` w = let stem = dropEnd 3 w in if containsVowel stem then post1b stem else w
  | otherwise       = w

-- | The shared cleanup after a successful @-ed@\/@-ing@ removal.
post1b :: String -> String
post1b stem
  | "at" `isSuf` stem = stem ++ "e"
  | "bl" `isSuf` stem = stem ++ "e"
  | "iz" `isSuf` stem = stem ++ "e"
  | endsDoubleConsonant stem && lastChar stem `notElem` ("lsz" :: String) = init stem
  | measure stem == 1 && endsCVC stem = stem ++ "e"
  | otherwise = stem

step1c :: String -> String
step1c w
  | "y" `isSuf` w && containsVowel (init w) = init w ++ "i"
  | otherwise = w

-- Porter helpers ------------------------------------------------------------

isSuf :: String -> String -> Bool
isSuf suf w = suf == drop (length w - length suf) w

dropEnd :: Int -> String -> String
dropEnd n w = take (length w - n) w

lastChar :: String -> Char
lastChar [] = ' '
lastChar xs = last xs

-- | Is the character at index @i@ a consonant? Porter's rule: A\/E\/I\/O\/U are
-- vowels; @y@ is a consonant at the start or after a vowel (and a vowel after a
-- consonant); every other letter is a consonant.
isConsonant :: String -> Int -> Bool
isConsonant w i =
  case w !! i of
    c | c `elem` ("aeiou" :: String) -> False
      | c == 'y' -> i == 0 || not (isConsonant w (i - 1))
      | otherwise -> True

-- | The Porter measure @m@: the number of vowel→consonant transitions in the
-- collapsed C\/V pattern of the word.
measure :: String -> Int
measure w = countVC (collapse [isConsonant w i | i <- [0 .. length w - 1]])
  where
    -- collapse adjacent equal flags into the run pattern (total; no head)
    collapse [] = []
    collapse (x : xs) = x : collapse (dropWhile (== x) xs)
    -- count vowel(False) immediately followed by consonant(True)
    countVC (False : True : rest) = 1 + countVC (True : rest)
    countVC (_ : rest)            = countVC rest
    countVC []                    = 0

containsVowel :: String -> Bool
containsVowel w = any (\i -> not (isConsonant w i)) [0 .. length w - 1]

endsDoubleConsonant :: String -> Bool
endsDoubleConsonant w =
  let n = length w
  in n >= 2 && (w !! (n - 1) == w !! (n - 2)) && isConsonant w (n - 1)

-- | Porter's @*o@: the stem ends consonant-vowel-consonant, where the final
-- consonant is not @w@, @x@ or @y@.
endsCVC :: String -> Bool
endsCVC w =
  let n = length w
  in n >= 3
       && isConsonant w (n - 3)
       && not (isConsonant w (n - 2))
       && isConsonant w (n - 1)
       && (w !! (n - 1)) `notElem` ("wxy" :: String)
