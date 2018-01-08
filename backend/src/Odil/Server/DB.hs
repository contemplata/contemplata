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
, fileMap
, fileSet
, fileSetFor
, accessLevel
, hasFile
, saveFile
, reSaveFile
, loadFile
, copyFile
, renameFile
, removeFile
-- ** Meta-related
, defaultMeta
, loadMeta
, saveMeta
, addAnnotator
, remAnnotator
, changeAccessAnnotator
-- *** Annotation status
, changeStatus
, startAnnotating
, postponeAnnotating
, finishAnnotating
-- , annoMap

-- * Low-level
, Register
, saveJSON
, loadJSON
, storeFilePath
) where


-- import qualified Control.Monad.IO.Class as IO
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad (when, unless)
import qualified Control.Exception as Exc
import qualified Control.Error as Err
import qualified Control.Monad.Trans.Reader as R

import           Data.Maybe (maybeToList)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as BLS
import qualified Data.Text as T
import qualified System.Directory as Dir
import System.FilePath ((</>), (<.>))

import Odil.Server.Types -- hiding (annoMap)
-- import qualified Odil.Server.Types as Types
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
  , authPath :: FilePath
    -- ^ Authorization path (relative w.r.t. `dbPath`)
  }


-- | A default config given a `dbPath`.
defaultConf :: FilePath -> DB
defaultConf dbPath = DB
  { dbPath = dbPath
  , regPath = Cfg.dbRegPath
  , storePath = Cfg.dbStorePath
  , authPath = Cfg.dbAuthPath }


---------------------------------------
---------------------------------------
-- DB objects: pure functions only
---------------------------------------
---------------------------------------


  -- | DB register. For the moment, the only meta-data that we need to know about
-- the files stored in the DB is... none. Actually, we just store a set of files
-- to annotate, we don't even know if some of them were already annotated or
-- not.
type Register = M.Map FileId FileMeta


-- | Does the register contain a given file?
regHasFile :: FileId -> Register -> Bool
regHasFile = M.member


-- | Add file.
regAddFile :: FileId -> FileMeta -> Register -> Register
regAddFile = M.insert


-- | Remove file.
regRemFile :: FileId -> Register -> Register
regRemFile = M.delete


-- | Get metadata.
regGetMeta :: FileId -> Register -> Maybe FileMeta
regGetMeta = M.lookup


-- -- | Update metadata.
-- regUpdateMeta :: FileId -> (FileMeta -> FileMeta) -> Register -> Register
-- regUpdateMeta = undefined


-- | Size of the register.
regSize :: Register -> Int
regSize = M.size


-- ---------------------------------------
-- -- Auth-related: pure functions only
-- ---------------------------------------
--
--
-- -- | DB register. For the moment, the only meta-data that we need to know about
-- -- the files stored in the DB is... none. Actually, we just store a set of files
-- -- to annotate, we don't even know if some of them were already annotated or
-- -- not.
-- type Register = S.Set FileId


---------------------------------------
---------------------------------------
-- DB monad
---------------------------------------
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
-- Top-level communication
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
fileMap :: DBT (M.Map FileId FileMeta)
fileMap = readReg


-- | Return the set of files stored in the DB.
fileSet :: DBT (S.Set FileId)
fileSet = M.keysSet <$> fileMap


-- | Check the access level of the given annotator for the given file.
accessLevel
  :: FileId
  -> AnnoName
  -> DBT (Maybe AccessLevel)
accessLevel fileName annoName = do
  theFileMap <- fileMap
  return $
    M.lookup annoName . annoMap =<<
    M.lookup fileName theFileMap


-- | Return the set of files with a corresponding access restriction.
fileSetFor
  :: AnnoName
  -> (AccessLevel -> Bool)
  -> DBT (S.Set FileId)
fileSetFor annoName hasAccess = do
  theFileMap <- fileMap
  return $ S.fromList
    [ fileId
    | (fileId, FileMeta{..}) <- M.toList theFileMap
    , accLev <- maybeToList (M.lookup annoName annoMap)
    , hasAccess accLev ]


-- | Add a file to a DB.
hasFile :: FileId -> DBT Bool
hasFile fid = do
  reg <- readReg
  return (regHasFile fid reg)


-- | Add a file to a DB.
saveFile :: FileId -> FileMeta -> File -> DBT ()
saveFile fid meta file = do
  reg <- readReg
  writeReg (regAddFile fid meta reg)
  storeSaveFile fid file


-- | Resave file. Metadata is copied if the file already existed, otherwise an
-- exception is raised.
reSaveFile :: FileId -> File -> DBT ()
reSaveFile fid file = do
  reg <- readReg
  meta <- case regGetMeta fid reg of
    Nothing -> Err.throwE "File ID does not exist"
    Just me -> return me
  writeReg (regAddFile fid meta reg)
  storeSaveFile fid file


-- | Load a file from a DB.
loadFile :: FileId -> DBT File
loadFile fid = do
  reg <- readReg
  unless (regHasFile fid reg)
    (Err.throwE "File ID does not exist")
  storeLoadFile fid


-- | Rename a DB file.
renameFile :: FileId -> FileId -> DBT ()
renameFile from to = do
  reg <- readReg
  meta <- case regGetMeta from reg of
    Nothing -> Err.throwE "File ID (from) does not exist"
    Just x  -> return x
  when (regHasFile to reg)
    (Err.throwE "File ID (to) already exists")
  writeReg . regAddFile to meta . regRemFile from $ reg
  storeRenameFile from to


-- | Similar to `renameFile`, but the original file is kept.
copyFile :: FileId -> FileId -> DBT ()
copyFile from to = do
  reg <- readReg
  unless (regHasFile from reg)
    (Err.throwE "File ID (from) does not exist")
  when (regHasFile to reg)
    (Err.throwE "File ID (to) already exists")
  writeReg $ regAddFile to defaultMeta reg
  storeCopyFile from to


-- | Remove a DB file.
removeFile :: FileId -> DBT ()
removeFile fileId = do
  reg <- readReg
  unless (regHasFile fileId reg)
    (Err.throwE "removeFile: file ID does not exist")
  writeReg . regRemFile fileId $ reg
  storeRemoveFile fileId


---------------------------------------
-- Top-level contd.
--
-- Meta-related
---------------------------------------


-- | Load a file from a DB.
loadMeta :: FileId -> DBT FileMeta
loadMeta fid = do
  reg <- readReg
  case regGetMeta fid reg of
    Nothing -> Err.throwE "File ID does not exist"
    Just me -> return me


-- | Load a file from a DB.
saveMeta :: FileId -> FileMeta -> DBT ()
saveMeta fid meta = do
  reg <- readReg
  writeReg $ regAddFile fid meta reg


-- | Add annotator to a given file.
addAnnotator :: FileId -> AnnoName -> AccessLevel -> DBT ()
addAnnotator fileName annoName accLev = do
  meta <- loadMeta fileName
  let newAnno = M.insert annoName accLev (annoMap meta)
      newMeta = meta {annoMap = newAnno}
  saveMeta fileName newMeta


-- | Add annotator to a given file.
remAnnotator :: FileId -> AnnoName -> DBT ()
remAnnotator fileName annoName = do
  meta <- loadMeta fileName
  let newAnno = M.delete annoName (annoMap meta)
      newMeta = meta {annoMap = newAnno}
  saveMeta fileName newMeta


-- | Add annotator to a given file.
changeAccessAnnotator :: FileId -> AnnoName -> DBT ()
changeAccessAnnotator fileName annoName =
  changeMeta fileName $ \meta -> do
    let accLevMay = M.lookup annoName (annoMap meta)
        turnLevel = \case
          Read -> Write
          Write -> Read
    return $ case accLevMay of
      Nothing -> meta
      Just x ->
        let newAnno = M.insert annoName (turnLevel x) (annoMap meta)
        in  meta {annoMap = newAnno}


-- | Add annotator to a given file.
startAnnotating :: FileId -> DBT ()
startAnnotating fileId = changeStatus fileId $ \status ->
  if status == New
  then Touched
  else status


-- | Add annotator to a given file.
postponeAnnotating :: FileId -> DBT ()
postponeAnnotating fileId = changeStatus fileId $ \status ->
  if status == Touched
  then New
  else status


-- | Add annotator to a given file.
finishAnnotating :: FileId -> DBT ()
finishAnnotating fileId = changeStatus fileId $ const Done


changeStatus :: FileId -> (FileStatus -> FileStatus) -> DBT ()
changeStatus fileId update =
  changeMeta fileId $ \meta -> do
    let newStatus = update (fileStatus meta)
    return $ meta {fileStatus = newStatus}


-- | Add annotator to a given file.
changeMeta :: FileId -> (FileMeta -> DBT FileMeta) -> DBT ()
changeMeta fileName metaModif = do
  meta <- loadMeta fileName
  newMeta <- metaModif meta
  saveMeta fileName newMeta


---------------------------------------
-- Top-level contd.
--
-- Auth-related
---------------------------------------


-- -- | Return the set of the annotators which have right to modify
-- -- the given file.
-- annoMap :: FileId -> DBT (S.Set FileId)
-- annoMap fileId = do
--   reg <- readReg
--   return $ case M.lookup fileId reg of
--     Nothing -> S.empty
--     Just meta -> Types.annoMap meta


---------------------------------------
-- IO: Register
---------------------------------------


-- | Create a new `Register` under a given filepath.
createReg :: DBT ()
createReg = writeReg M.empty


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


-- | Rename a given file in the store.
storeRenameFile
  :: FileId -- ^ From
  -> FileId -- ^ To
  -> DBT ()
storeRenameFile from to = do
  pathFrom <- storeFilePath from
  pathTo <- storeFilePath to
  liftIO $ Dir.renameFile pathFrom pathTo


-- | Copy a given file in the store.
storeCopyFile
  :: FileId -- ^ From
  -> FileId -- ^ To
  -> DBT ()
storeCopyFile from to = do
  pathFrom <- storeFilePath from
  pathTo <- storeFilePath to
  liftIO $ Dir.copyFile pathFrom pathTo


-- | Remove a given file from the store.
storeRemoveFile :: FileId -> DBT ()
storeRemoveFile fileId = do
  path <- storeFilePath fileId
  liftIO $ Dir.removeFile path


storeFilePath :: FileId -> DBT FilePath
storeFilePath fid = do
  DB{..} <- dbConf
  return $ dbPath </> storePath </> T.unpack (encodeFileId fid) <.> "json"


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
