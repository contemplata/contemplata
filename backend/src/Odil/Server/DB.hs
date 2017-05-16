{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}


-- | Annotation server database.


module Odil.Server.DB
(
) where


-- import qualified Control.Monad.IO.Class as IO
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import qualified Control.Exception as Exc
import qualified Control.Error as Err

import qualified Data.Set as S
import qualified Data.ByteString.Lazy as BLS
import qualified Data.Text as T
import qualified System.Directory as Dir
import System.FilePath ((</>))

import Odil.Server.Types
import qualified Odil.Server.Config as Cfg
import qualified Data.Aeson as JSON


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


-- | Database config.
data DB = DB
  { dbPath :: FilePath
    -- ^ Directory in which the DB is stored.
  , regPath :: FilePath
    -- ^ Register file.
  }


---------------------------------------
-- DB monad
---------------------------------------


-- | DB monad transformer.
type DBT = Err.ExceptT Err IO


-- | An error message.
type Err = T.Text


-- | Run a given DBT computation.
runDBT :: DBT a -> IO (Either Err a)
runDBT = Err.runExceptT . try


---------------------------------------
-- IO communication
---------------------------------------


-- | Create an empty DB under a specified directory, unless it already exists.
-- Return True on success.
createDB :: DB -> DBT ()
createDB DB{..} = do
  liftIO (Dir.doesPathExist dbPath) >>= \case
    True -> Err.throwE "Directory exists"
    False -> do
      liftIO $ Dir.createDirectoryIfMissing True dbPath
      createReg (dbPath </> regPath)


-- | Add a file to a DB.
addFile :: DB -> FileId -> File -> DBT ()
addFile DB{..} fid file = do
  reg <- readReg regPath
  when (regHasFile fid reg)
    (Err.throwE "File ID already exists")
  Err.throwE "Implement writing the file on disk!"
  writeReg regPath (regAddFile fid reg)


---------------------------------------
-- Register
---------------------------------------


-- | Create a new `Register` under a given filepath.
createReg :: FilePath -> DBT ()
createReg = flip writeReg S.empty


-- | Read the register file.
readReg :: FilePath -> DBT Register
readReg regPath = do
  cts <- liftIO $ BLS.readFile regPath
  Err.hoistEither . onLeft T.pack $ JSON.eitherDecode' cts


-- | Read the register file.
writeReg :: FilePath -> Register -> DBT ()
writeReg regPath reg = liftIO . BLS.writeFile regPath . JSON.encode $ reg


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


-- | Try to perform a DBT computation and catch any exceptions that might happen
-- along the way.
try :: DBT a -> DBT a
try (Err.ExceptT io) = do
  x <- liftIO (Exc.try io)
  case x of
    Left err -> Err.throwE (displayException err)
    Right errVal -> Err.hoistEither errVal
  -- Err.hoistEither . onLeft displayException =<< IO.liftIO (Exc.try io)

--   = Err.hoistEither
--   . onLeft displayException
--   =<< IO.liftIO (Exc.try c)


-- | Map a function over the left value only.
onLeft :: (a -> b) -> Either a c -> Either b c
onLeft f (Left x) = Left (f x)
onLeft f (Right x) = Right x


displayException :: Exc.SomeException -> T.Text
displayException = T.pack . Exc.displayException
