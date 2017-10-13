{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}


module Main where


import Control.Monad (when, forM_, forM)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.State as State
import qualified Control.Arrow as Arr

import qualified Control.Error as Err
import qualified System.FilePath as FilePath
import System.FilePath ((</>), (<.>))
import qualified System.Directory as Dir
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as T
-- import qualified Data.Set as S
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as JSON
import Data.Function (on)
import Data.Monoid ((<>))
import Data.Maybe (mapMaybe)
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
import qualified Odil.FTB as FTB
import qualified Odil.Stanford as Stanford
import qualified Odil.DiscoDOP as DiscoDOP


--------------------------------------------------
-- Commands
--------------------------------------------------


data Command

    = Simplify FilePath
      -- ^ Parse and show the sentences in the Ancor XML file

    | Preprocess (Maybe FilePath)
      -- ^ Preprocess the sentence for parsing; the optional argument specifies
      -- the path to the external file with sentences to be removed

    | Process (Maybe FilePath)
      -- ^ Ancor file -> ODIL (in JSON); the optional argument specifies the
      -- path to the external file with sentences to be removed

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

    | Penn2JSON
      -- ^ Convert a PTB-style file to JSON

    | RenameFileDB String String FilePath
      -- ^ Rename a file in a given DB

    | CopyFileDB String String FilePath
      -- ^ Copy a file in a given DB

    | StatsDB FilePath
      -- ^ Print statistics related to the DB

    | FTB2Penn FilePath Bool (Maybe FilePath)
      -- ^ Convert a FTB XML file to the Penn format

    | UpdateMeta1 FilePath
      -- ^ Update meta information in the given DB (in-place)


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
preprocessOptions = Preprocess
  <$> (optional . strOption)
        ( long "remove"
       <> short 'r'
       <> metavar "FILE"
       <> help "" )


processOptions :: Parser Command
processOptions = Process
  <$> (optional . strOption)
        ( long "remove"
       <> short 'r'
       <> metavar "FILE"
       <> help "" )


penn2jsonOptions :: Parser Command
penn2jsonOptions = pure Penn2JSON


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


renameFileDbOptions :: Parser Command
renameFileDbOptions = RenameFileDB
  <$> strOption
        ( long "from"
       <> short 'f'
       <> metavar "NAME-FROM"
       <> help "Name of the file in the DB" )
  <*> strOption
        ( long "to"
       <> short 't'
       <> metavar "NAME-TO"
       <> help "The new name" )
  <*> strOption
        ( long "dbdir"
       <> short 'd'
       <> metavar "DIR"
       <> help "DB directory" )


copyFileDbOptions :: Parser Command
copyFileDbOptions = CopyFileDB
  <$> strOption
        ( long "from"
       <> short 'f'
       <> metavar "NAME-FROM"
       <> help "Name of the file in the DB" )
  <*> strOption
        ( long "to"
       <> short 't'
       <> metavar "NAME-TO"
       <> help "The new name" )
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


ftb2pennOptions :: Parser Command
ftb2pennOptions = FTB2Penn
  <$> strOption
        ( long "file"
       <> short 'f'
       <> metavar "XML"
       <> help "FTB XML (original format) file" )
  <*> switch
        ( long "remove-punctuation"
       <> short 'r'
       <> help "Remove punctuation (with the expcetion of question marks)" )
  <*> (optional . strOption)
        ( long "outdir"
       <> short 'o'
       <> metavar "DIR"
       <> help "Output directory (for more fine-grained output)" )


updateMetaOptions :: (FilePath -> Command) -> Parser Command
updateMetaOptions updateMeta = updateMeta
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
  <> command "renamefiledb"
    (info (helper <*> renameFileDbOptions)
      (progDesc "Rename a file in a DB")
    )
  <> command "copyfiledb"
    (info (helper <*> copyFileDbOptions)
      (progDesc "Copy a file in a DB")
    )
  <> command "statsdb"
    (info (helper <*> statsDbOptions)
      (progDesc "Print DB-related statistics")
    )
  <> command "ftb2penn"
    (info (helper <*> ftb2pennOptions)
      (progDesc "Convert a FTB XML file to the Penn format")
    )
  <> command "penn2json"
    (info (helper <*> penn2jsonOptions)
      (progDesc "Convert a PTB-style file to JSON")
    )
  <> command "updatemeta1"
    (info (helper <*> updateMetaOptions UpdateMeta1)
      (progDesc "Update files' metadata information")
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
    Preprocess rmFile -> do
      sentences <- T.lines <$> T.getContents
      prepare <- Pre.prepare <$> case rmFile of
        Nothing -> pure []
        Just path -> Pre.readConfig path
      T.putStr . T.unlines . map prepare $ sentences
--     Penn2Odil pennPath origPath -> do
--       penn <- Penn.parseForest <$> T.readFile pennPath
--       orig <- T.lines <$> T.readFile origPath
--       let file = Penn.convertPennFile (zip orig penn)
--       LBS.putStr (JSON.encode file)

    -- Read the ancor file from stdin and output the resulting
    -- ODIL file in the JSON format
    Process rmFile -> do
      ancorFile <- T.getContents
      let ancor = Parse.parseTrans ancorFile
      (turns2, treeMap) <- flip State.runStateT M.empty $ do
        forM ancor $ \section -> do
          forM section $ \turn -> do
            treeList <- forM (Ancor.elems turn) $ \(mayWho, elem) -> do
              let sent = Show.showElem elem
                  prepare = Pre.prepare []
              prepare <- Pre.prepare <$> case rmFile of
                Nothing -> pure []
                Just path -> liftIO $ Pre.readConfig path
              penn <- liftIO $ Stanford.parseFR (prepare sent) >>= \case
                Nothing -> do
                  let try err cmd = cmd >>= \case
                        Nothing -> error err
                        Just x  -> return x
                  -- error "A problem occurred with the Stanford parser!"
                  toks <- try "Didn't manage to tokenize with Stanford" $
                    Stanford.posTagFR (prepare sent)
                  -- try "None of the parsers managed to parse the sentence" $
                  --   DiscoDOP.tagParseDOP Nothing (map fst toks)
                  return $ dummyTree toks
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
            , linkSet = M.empty
            }
      LBS.putStr (JSON.encode file)

    -- Read the ancor file from stdin and output the resulting
    -- ODIL file in the JSON format
    Penn2JSON -> do
      -- pennForest <- Penn.parseForest <$> T.getContents
      pennForest <- map Penn.parseTree . T.lines <$> T.getContents
--       (turns, treeMap) <- flip State.runStateT M.empty $ do
--         forM pennForest $ \penn -> do
--           let odil = Penn.toOdilTree penn
--               sent = ""
--           k <- State.gets $ (+1) . M.size
--           State.modify' $ M.insert k (sent, odil)
--           -- return (k, Nothing) -- speaker N/A
--           return $ Odil.Turn
--             { speaker = []      -- speakers N/A
--             , trees = M.singleton k Nothing } -- speaker N/A
--       let file = Odil.File
--             { treeMap = treeMap
--             , turns = turns -- concat turns
--             , linkSet = M.empty
--             }
      let odil = Penn.convertPennFile pennForest
      LBS.putStr (JSON.encode odil)


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
          Right json -> DB.saveFile fileId Odil.defaultMeta json
      case res of
        Left err -> T.putStrLn $ "Operation failed: " `T.append` err
        Right _  -> return ()
    RenameFileDB from to dbPath -> do
      let dbConf = DB.defaultConf dbPath
      res <- DB.runDBT dbConf $ do
        DB.renameFile (T.pack from) (T.pack to)
      case res of
        Left err -> T.putStrLn $ "Operation failed: " `T.append` err
        Right _  -> return ()
    CopyFileDB from to dbPath -> do
      let dbConf = DB.defaultConf dbPath
      res <- DB.runDBT dbConf $ do
        DB.copyFile (T.pack from) (T.pack to)
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

    -- Misc
    FTB2Penn filePath rmPunc outPathMay -> do
      let process =
            if rmPunc
            then mapMaybe (\(i, t) -> (,) <$> pure i <*> FTB.rmPunc t)
            else id
      pennPairs <- map (Arr.second FTB.toPenn)
                . process
                . FTB.parseFTB
              <$> T.readFile filePath
      case outPathMay of
        Nothing -> do
          forM_ pennPairs $ \(_, penn) -> do
            T.putStrLn . Penn.showTree $ penn
        Just dirPath -> do
          res <- Err.runExceptT $ do
            liftIO (Dir.doesPathExist dirPath) >>= \case
              True -> Err.throwE . T.pack $ "the directory '" ++ dirPath ++ "' exists"
              False -> return ()
            liftIO $ do
              Dir.createDirectoryIfMissing True dirPath
            let pennGrps = L.groupBy ((==) `on` fst) pennPairs
                fileName = FilePath.takeBaseName filePath
            forM_ pennGrps $ \pennGrp -> do
              let (textIDs, pennList) = unzip pennGrp
              textID <- case textIDs of
                [] -> Err.throwE "L.groupBy worked unexpectedly"
                i : _ -> return i
              let pennStr = T.unlines (map Penn.showTree pennList)
              liftIO $ do
                let outFilePath = dirPath </> fileName ++ "-" ++ T.unpack textID <.> "ptb"
                T.writeFile outFilePath pennStr
          case res of
            Left err -> T.putStrLn $ "Error: " `T.append` err
            Right _  -> return ()

    UpdateMeta1 dbPath -> do
      let dbConf = DB.defaultConf dbPath
      res <- DB.runDBT dbConf $ do
        -- manaully loading the old version of the database
        let path = DB.dbPath dbConf </> DB.regPath dbConf
        oldReg <- DB.loadJSON path
        let newReg = M.fromList
              [ (x, Odil.defaultMeta)
              | x <- S.toList oldReg ]
        DB.saveJSON path (newReg :: DB.Register)
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


-- | Create a dummy tree from a list of words and their POS tags.
dummyTree :: [(Stanford.Orth, Stanford.Pos)] -> Penn.Tree
dummyTree =
  R.Node "ROOT" . map mkLeaf
  where
    mkLeaf (orth, pos) = R.Node pos [R.Node orth []]
