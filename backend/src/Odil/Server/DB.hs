{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}


-- | Annotation server database.


module Odil.Server.DB
(
-- * Configuration
  DB (..)
, defaultConf

-- * DB monad transformer
, DBT
, runDBT

-- * DB top-level
, createDB
, fileNum
, fileSet
, hasFile
, saveFile
, loadFile
) where


-- import qualified Control.Monad.IO.Class as IO
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad (when, unless)
import qualified Control.Exception as Exc
import qualified Control.Error as Err
import qualified Control.Monad.Trans.Reader as R

import qualified Data.Set as S
import qualified Data.ByteString.Lazy as BLS
import qualified Data.Text as T
import qualified System.Directory as Dir
import System.FilePath ((</>), (<.>))

import Odil.Server.Types
import qualified Odil.Server.Config as Cfg
import qualified Data.Aeson as JSON


---------------------------------------
-- DB config
---------------------------------------


-- | Database config.
data DB = DB
  { dbPath :: FilePath
    -- ^ Directory in which the DB is stored.
  , regPath :: FilePath
    -- ^ Register path (relative w.r.t. `dbPath`)
  , storePath :: FilePath
    -- ^ Store (files) path (relative w.r.t. `dbPath`)
  }


-- | A default config given a `dbPath`.
defaultConf :: FilePath -> DB
defaultConf dbPath = DB
  { dbPath = dbPath
  , regPath = Cfg.dbRegPath
  , storePath = Cfg.dbStorePath }


---------------------------------------
-- DB objects: pure functions only
---------------------------------------


-- | DB register. For the moment, the only meta-data that we need to know about
-- the files stored in the DB is... none. Actually, we just store a set of files
-- to annotate, we don't even know if some of them were already annotated or
-- not.
type Register = S.Set FileId


-- | Does the register contain a given file?
regHasFile :: FileId -> Register -> Bool
regHasFile = S.member


-- | Does the register contain a given file?
regAddFile :: FileId -> Register -> Register
regAddFile = S.insert


-- | Size of the register.
regSize :: Register -> Int
regSize = S.size


---------------------------------------
-- DB monad
---------------------------------------


-- | DB monad transformer.
type DBT = Err.ExceptT Err (R.ReaderT DB IO)
-- type DBT = Err.ExceptT Err IO


-- | An error message.
type Err = T.Text


-- | Run a given DBT computation. Try to catch any exceptions that might happen
-- along the way and return them as errors (`Left`).
runDBT :: DB -> DBT a -> IO (Either Err a)
runDBT db dbt = do
  x <- Exc.try . flip R.runReaderT db . Err.runExceptT $ dbt
  case x of
    Left err -> return . Left . displayException $ err
    Right errVal -> return errVal


-- | Get the DB configuration.
dbConf :: DBT DB
dbConf = lift R.ask


-- -- | Get the DB configuration.
-- dbConfOn :: (DB -> a) ->  DBT a
-- dbConfOn f = lift (R.asks f)


---------------------------------------
-- Top-level IO communication
---------------------------------------


-- | Create an empty DB under a specified directory, unless it already exists.
-- Return True on success.
createDB :: DBT ()
createDB = do
  DB{..} <- dbConf
  liftIO (Dir.doesPathExist dbPath) >>= \case
    True -> Err.throwE . T.pack $ "the directory '" ++ dbPath ++ "' exists"
    False -> do
      liftIO $ do
        Dir.createDirectoryIfMissing True dbPath
        Dir.createDirectoryIfMissing True (dbPath </> storePath)
      createReg


-- | Return the number of files stored in the DB.
fileNum :: DBT Int
fileNum = regSize <$> readReg


-- | Return the set of files stored in the DB.
fileSet :: DBT (S.Set FileId)
fileSet = readReg


-- | Add a file to a DB.
hasFile :: FileId -> DBT Bool
hasFile fid = do
  reg <- readReg
  return (regHasFile fid reg)


-- | Add a file to a DB.
saveFile :: FileId -> File -> DBT ()
saveFile fid file = do
  reg <- readReg
  -- when (regHasFile fid reg)
  --   (Err.throwE "File ID already exists")
  writeReg (regAddFile fid reg)
  storeSaveFile fid file


-- | Load a file from a DB.
loadFile :: FileId -> DBT File
loadFile fid = do
  reg <- readReg
  unless (regHasFile fid reg)
    (Err.throwE "File ID does not exist")
  storeLoadFile fid


---------------------------------------
-- IO: Register
---------------------------------------


-- | Create a new `Register` under a given filepath.
createReg :: DBT ()
createReg = writeReg S.empty


-- | Read the register file.
readReg :: DBT Register
readReg = do
  DB{..} <- dbConf
  loadJSON (dbPath </> regPath)


-- | Read the register file.
writeReg :: Register -> DBT ()
writeReg reg = do
  DB{..} <- dbConf
  saveJSON (dbPath </> regPath) reg
  -- liftIO . BLS.writeFile regPath . JSON.encode $ reg


---------------------------------------
-- IO: Files
---------------------------------------


-- | Store a given file in the store.
storeSaveFile :: FileId -> File -> DBT ()
storeSaveFile fid file =
  flip saveJSON file =<< storeFilePath fid


-- | Store a given file in the store.
storeLoadFile :: FileId -> DBT File
storeLoadFile fid = loadJSON =<< storeFilePath fid


storeFilePath :: FileId -> DBT FilePath
storeFilePath fid = do
  DB{..} <- dbConf
  return $ dbPath </> storePath </> T.unpack fid <.> "json"


---------------------------------------
-- JSON
---------------------------------------


-- | Load a JSON document from a given file.
loadJSON :: JSON.FromJSON a => FilePath -> DBT a
loadJSON path = do
  cts <- liftIO $ BLS.readFile path
  Err.hoistEither . onLeft T.pack $ JSON.eitherDecode' cts


-- | Load a JSON document from a given file.
saveJSON :: JSON.ToJSON a => FilePath -> a -> DBT ()
saveJSON path = liftIO . BLS.writeFile path . JSON.encode


---------------------------------------
-- Utils
---------------------------------------


-- -- | Try an IO computation in a DBT monad.
-- --
-- -- NOTE: we may be abusing it here, maybe it would be enough to use it top-level?
-- -- So far, I have not found a clear and nice way to do that, though.
-- liftIO :: IO a -> DBT a
-- liftIO c
--   = Err.hoistEither
--   . onLeft displayException
--   =<< IO.liftIO (Exc.try c)


-- -- | Try to perform a DBT computation and catch any exceptions that might happen
-- -- along the way.
-- try :: DBT a -> DBT a
-- try (Err.ExceptT m) = do
--   x <- liftIO . Exc.try $ m
--   case x of
--     Left err -> Err.throwE (displayException err)
--     Right errVal -> Err.hoistEither errVal
--   -- Err.hoistEither . onLeft displayException =<< IO.liftIO (Exc.try io)
--
-- --   = Err.hoistEither
-- --   . onLeft displayException
-- --   =<< IO.liftIO (Exc.try c)


-- | Map a function over the left value only.
onLeft :: (a -> b) -> Either a c -> Either b c
onLeft f (Left x) = Left (f x)
onLeft f (Right x) = Right x


displayException :: Exc.SomeException -> T.Text
displayException = T.pack . Exc.displayException
