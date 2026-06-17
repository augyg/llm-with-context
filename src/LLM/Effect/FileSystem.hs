{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | A narrow 'FileSystem' effect with just the operations the budget interpreter
-- needs: existence check, directory creation, atomic JSON read/write. Defined
-- here so the public Budget interpreter signature stays free of 'IOE'.
module LLM.Effect.FileSystem
  ( -- * Effect
    FileSystem (..)
    -- * Operations
  , doesFileExist
  , createDirectoryIfMissing
  , readJSONFile
  , atomicWriteJSONFile
    -- * Interpreter
  , runFileSystemIO
  ) where

import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy as BL
import qualified System.Directory as Dir
import System.FilePath (takeDirectory, (<.>))

import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.Internal.Monad (unsafeEff_)

data FileSystem :: Effect where
  DoesFileExist           :: FilePath -> FileSystem m Bool
  CreateDirectoryIfMissing :: Bool -> FilePath -> FileSystem m ()
  ReadJSONBytes           :: FilePath -> FileSystem m (Either String BL.ByteString)
  AtomicWriteBytes        :: FilePath -> BL.ByteString -> FileSystem m ()

type instance DispatchOf FileSystem = Dynamic

doesFileExist :: (FileSystem :> es) => FilePath -> Eff es Bool
doesFileExist p = send (DoesFileExist p)

createDirectoryIfMissing :: (FileSystem :> es) => Bool -> FilePath -> Eff es ()
createDirectoryIfMissing parents p = send (CreateDirectoryIfMissing parents p)

-- | Decode a JSON file. Returns 'Left' on missing file, IO error, or decode error.
readJSONFile :: (FileSystem :> es, FromJSON a) => FilePath -> Eff es (Either String a)
readJSONFile p = do
  result <- send (ReadJSONBytes p)
  pure $ result >>= Aeson.eitherDecode'

-- | Encode and write a JSON file atomically (temp + rename). Creates the
-- containing directory if missing.
atomicWriteJSONFile :: (FileSystem :> es, ToJSON a) => FilePath -> a -> Eff es ()
atomicWriteJSONFile p v = do
  send (CreateDirectoryIfMissing True (takeDirectory p))
  send (AtomicWriteBytes p (Aeson.encode v))

runFileSystemIO :: Eff (FileSystem : es) a -> Eff es a
runFileSystemIO = interpret $ \_ -> \case
  DoesFileExist p -> unsafeEff_ (Dir.doesFileExist p)
  CreateDirectoryIfMissing parents p ->
    unsafeEff_ (Dir.createDirectoryIfMissing parents p)
  ReadJSONBytes p -> unsafeEff_ $ do
    exists <- Dir.doesFileExist p
    if not exists
      then pure (Left ("file not found: " <> p))
      else fmap Right (BL.readFile p)
  AtomicWriteBytes p bs -> unsafeEff_ $ do
    let tmp = p <.> "tmp"
    BL.writeFile tmp bs
    Dir.renameFile tmp p
