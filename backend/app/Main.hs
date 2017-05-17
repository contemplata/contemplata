{-# LANGUAGE OverloadedStrings #-}


module Main where


import Control.Monad.IO.Class (liftIO)
import qualified Control.Error as Err
import qualified System.FilePath as FilePath
import qualified Data.Text as T
import qualified Data.Text.IO as T
-- import qualified Data.Set as S
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as JSON
import Data.Monoid ((<>))
import Options.Applicative

import qualified Odil.Ancor.IO.Parse as Parse
import qualified Odil.Ancor.IO.Show as Show
import qualified Odil.Server.Types as Odil
import qualified Odil.Server.DB as DB
import qualified Odil.Server as Server
import qualified Odil.Penn as Penn


--------------------------------------------------
-- Commands
--------------------------------------------------


data Command
    = Simplify FilePath
      -- ^ Parse and show the sentences in the Ancor XML file
    | Penn2Odil
      -- ^ Convert the Penn file on input to an JSON file
    | Server FilePath
      -- ^ Run the backend annotation server
    | NewDB FilePath
      -- ^ Create a new DB under a given path
    | AddFileDB FilePath FilePath
      -- ^ Add a new file to a given DB


--------------------------------------------------
-- Options
--------------------------------------------------


simplifyOptions :: Parser Command
simplifyOptions = Simplify
  <$> strOption
        ( long "ancor"
       <> short 'a'
       <> metavar "FILE"
       <> help "Ancor .xml file" )


penn2odilOptions :: Parser Command
penn2odilOptions = pure Penn2Odil


serverOptions :: Parser Command
serverOptions = Server
  <$> strOption
        ( long "dbdir"
       <> short 'd'
       <> metavar "DIR"
       <> help "DB directory" )


newDbOptions :: Parser Command
newDbOptions = NewDB
  <$> strOption
        ( long "dbdir"
       <> short 'd'
       <> metavar "DIR"
       <> help "DB directory" )


addFileDbOptions :: Parser Command
addFileDbOptions = AddFileDB
  <$> strOption
        ( long "file"
       <> short 'f'
       <> metavar "FILE"
       <> help "File to add to DB" )
  <*> strOption
        ( long "dbdir"
       <> short 'd'
       <> metavar "DIR"
       <> help "DB directory" )


opts :: Parser Command
opts = subparser
  ( command "simplify"
    (info (helper <*> simplifyOptions)
      (progDesc "Parse and show the sentences in the Ancor XML file")
    )
  <> command "penn2odil"
    (info (helper <*> penn2odilOptions)
      (progDesc "Convert Penn trees to Odil trees in JSON")
    )
  <> command "server"
    (info (helper <*> serverOptions)
      (progDesc "Run the backed annotation server")
    )
  <> command "new-db"
    (info (helper <*> newDbOptions)
      (progDesc "Create a new DB under a given path")
    )
  <> command "add-file-db"
    (info (helper <*> addFileDbOptions)
      (progDesc "Add a new file to a DB")
    )
  )


-- | Run program depending on the cmdline arguments.
run :: Command -> IO ()
run cmd =
  case cmd of

    -- Various commands
    Simplify ancorFile -> do
      file <- T.readFile ancorFile
      T.putStrLn . Show.showAncor . Parse.parseTrans $ file
    Penn2Odil -> do
      file <- Penn.convertPennFile . Penn.parseForest <$> T.getContents
      LBS.putStr (JSON.encode file)
      -- T.putStrLn . Show.showAncor . Parse.parseTrans $ file

    -- Server-related
    Server dbPath -> Server.runServer dbPath

    -- DB-related
    NewDB dbPath -> do
      let dbConf = DB.defaultConf dbPath
      res <- DB.runDBT dbConf $ do
        DB.createDB
      case res of
        Left err -> T.putStrLn $ "Could not create DB: " `T.append` err
        Right _  -> return ()
    AddFileDB filePath dbPath -> do
      let dbConf = DB.defaultConf dbPath
      res <- DB.runDBT dbConf $ do
        let fileId = T.pack (FilePath.takeBaseName filePath)
        cs <- liftIO $ LBS.readFile filePath
        case JSON.eitherDecode cs of
          Left err -> Err.throwE "JSON decoding failed"
          Right file -> DB.saveFile fileId file
      case res of
        Left err -> T.putStrLn $ "Could not save the file: " `T.append` err
        Right _  -> return ()



-- saveFile :: FileId -> File -> DBT ()


main :: IO ()
main =
    execParser optsExt >>= run
  where
    optsExt = info (helper <*> opts)
       ( fullDesc
      <> progDesc "Working with ODIL files"
      <> header "odil" )
