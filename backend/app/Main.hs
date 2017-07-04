{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}


module Main where


import Control.Monad (when, forM_, forM)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.State as State

import qualified Control.Error as Err
import qualified System.FilePath as FilePath
import qualified Data.Text as T
import qualified Data.Text.IO as T
-- import qualified Data.Set as S
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as JSON
import Data.Monoid ((<>))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Tree as R
import Options.Applicative

import qualified Odil.Ancor.Types as Ancor
import qualified Odil.Ancor.IO.Parse as Parse
import qualified Odil.Ancor.IO.Show as Show
import qualified Odil.Ancor.Preprocess as Pre
import qualified Odil.Server.Types as Odil
import qualified Odil.Server.Config as ServerCfg
import qualified Odil.Server.DB as DB
import qualified Odil.Server as Server
import qualified Odil.Penn as Penn
import qualified Odil.Stanford as Stanford


--------------------------------------------------
-- Commands
--------------------------------------------------


data Command
    = Simplify FilePath
      -- ^ Parse and show the sentences in the Ancor XML file
    | Preprocess
      -- ^ Preprocess the sentence for parsing
    | Process
      -- ^ Ancor file -> ODIL (in JSON)
--     | Penn2Odil
--         FilePath -- ^ Penn file
--         FilePath -- ^ File with original sentences
--       -- ^ Convert a Penn file to a JSON file
    | Server FilePath String Int
      -- ^ Run the backend annotation server
    | NewDB FilePath
      -- ^ Create a new DB under a given path
    | AddFileDB Bool FilePath FilePath
      -- ^ Add a new file to a given DB
    | StatsDB FilePath
      -- ^ Print statistics related to the DB


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


preprocessOptions :: Parser Command
preprocessOptions = pure Preprocess


processOptions :: Parser Command
processOptions = pure Process


-- penn2odilOptions :: Parser Command
-- penn2odilOptions = Penn2Odil
--   <$> strOption
--         ( long "penn"
--        <> short 'p'
--        <> metavar "FILE"
--        <> help "Penn file" )
--   <*> strOption
--         ( long "orig"
--        <> short 'o'
--        <> metavar "FILE"
--        <> help "File with original sentences" )


serverOptions :: Parser Command
serverOptions = Server
  <$> strOption
        ( long "dbdir"
       <> short 'd'
       <> metavar "DIR"
       <> help "DB directory" )
  <*> strOption
        ( long "url"
       <> short 'u'
       <> metavar "URL"
       <> value ServerCfg.serverAddr
       <> help "Address to bind (WebSocket server)" )
  <*> option auto
        ( long "port"
       <> short 'p'
       <> metavar "INT"
       <> value ServerCfg.serverPort
       <> help "Port to listen to (WebSocket server)" )


newDbOptions :: Parser Command
newDbOptions = NewDB
  <$> strOption
        ( long "dbdir"
       <> short 'd'
       <> metavar "DIR"
       <> help "DB directory" )


addFileDbOptions :: Parser Command
addFileDbOptions = AddFileDB
  <$> switch
        ( long "force"
       <> short 'f'
       <> help "Add file even if already present in DB" )
  <*> strOption
        ( long "json"
       <> short 'j'
       <> metavar "JSON"
       <> help "JSON file to add to DB" )
  <*> strOption
        ( long "dbdir"
       <> short 'd'
       <> metavar "DIR"
       <> help "DB directory" )


statsDbOptions :: Parser Command
statsDbOptions = StatsDB
  <$> strOption
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
  <> command "preprocess"
    (info (helper <*> preprocessOptions)
      (progDesc "Prepare sentences (one per line on <stdin>) for parsing")
    )
  <> command "process"
    (info (helper <*> processOptions)
      (progDesc "Full processing pipeline (Ancor file -> ODIL JSON)")
    )
--   <> command "penn2odil"
--     (info (helper <*> penn2odilOptions)
--       (progDesc "Convert Penn trees to Odil trees in JSON")
--     )
  <> command "server"
    (info (helper <*> serverOptions)
      (progDesc "Run the backed annotation server")
    )
  <> command "createdb"
    (info (helper <*> newDbOptions)
      (progDesc "Create a new DB under a given path")
    )
  <> command "addfiledb"
    (info (helper <*> addFileDbOptions)
      (progDesc "Add a new file to a DB")
    )
  <> command "statsdb"
    (info (helper <*> statsDbOptions)
      (progDesc "Print DB-related statistics")
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
    Preprocess -> do
      sentences <- T.lines <$> T.getContents
      T.putStr . T.unlines . map Pre.prepare $ sentences
--     Penn2Odil pennPath origPath -> do
--       penn <- Penn.parseForest <$> T.readFile pennPath
--       orig <- T.lines <$> T.readFile origPath
--       let file = Penn.convertPennFile (zip orig penn)
--       LBS.putStr (JSON.encode file)

    -- Read the ancor file from stdin and output the resulting
    -- ODIL file in the JSON format
    Process -> do
      ancorFile <- T.getContents
      let ancor = Parse.parseTrans ancorFile
      (turns2, treeMap) <- flip State.runStateT M.empty $ do
        forM ancor $ \section -> do
          forM section $ \turn -> do
            treeList <- forM (Ancor.elems turn) $ \(mayWho, elem) -> do
              let sent = Show.showElem elem
              penn <- liftIO $ Stanford.parseFR (Pre.prepare sent) >>= \case
                Nothing -> error "A problem occurred with the Stanford parser!"
                Just x -> return x
              let odil = Penn.toOdilTree penn
              k <- State.gets $ (+1) . M.size
              State.modify' $ M.insert k (sent, odil)
              return (k, fmap Ancor.unWho mayWho)
            return $ Odil.Turn
              { speaker = Ancor.speaker turn
              , trees = M.fromList treeList }
      let file = Odil.File
            { treeMap = treeMap
            , turns = concat turns2
            , linkSet = S.empty
            }
      LBS.putStr (JSON.encode file)


    -- Server-related
    Server dbPath addr port -> Server.runServer dbPath addr port

    -- DB-related
    NewDB dbPath -> do
      let dbConf = DB.defaultConf dbPath
      res <- DB.runDBT dbConf $ do
        DB.createDB
      case res of
        Left err -> T.putStrLn $ "Could not create DB: " `T.append` err
        Right _  -> return ()
    AddFileDB force jsonPath dbPath -> do
      let dbConf = DB.defaultConf dbPath
      res <- DB.runDBT dbConf $ do
        let fileId = T.pack (FilePath.takeBaseName jsonPath)
        when (not force) $ DB.hasFile fileId >>= \case
          True -> Err.throwE "file ID already present in DB"
          _ -> return ()
        cs <- liftIO $ LBS.readFile jsonPath
        case JSON.eitherDecode cs of
          Left err -> Err.throwE "JSON decoding failed"
          Right json -> DB.saveFile fileId json
      case res of
        Left err -> T.putStrLn $ "Operation failed: " `T.append` err
        Right _  -> return ()
    StatsDB dbPath -> do
      let dbConf = DB.defaultConf dbPath
      res <- DB.runDBT dbConf $ do
        ids <- S.toList <$> DB.fileSet
        forM_ ids $ \fileId -> do
          file <- DB.loadFile fileId
          liftIO . T.putStrLn . T.concat $
            [ fileId
            , " => "
            , T.pack (show $ numberOfLeavesF file) ]
      case res of
        Left err -> T.putStrLn $ "Operation failed: " `T.append` err
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


-------------------------------------------------------
-- Utils
-------------------------------------------------------


-- | Number of leaves in a given file.
numberOfLeavesF :: Odil.File -> Int
numberOfLeavesF Odil.File{..} = sum
  [ numberOfLeavesT tree
  | (treeId, (sent, tree)) <- M.toList treeMap ]


-- | Number of leaves in a given tree.
numberOfLeavesT :: Odil.Tree -> Int
numberOfLeavesT
  = length
  . filter isLeaf
  . R.flatten
  where
    isLeaf x = case x of
      Odil.Leaf{} -> True
      _ -> False
