{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}


module Main where


import Control.Monad (when, forM_, forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as State
import           Control.Monad.Trans.Maybe (runMaybeT, MaybeT(..))
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
import Data.Maybe (mapMaybe, catMaybes)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Tree as R
import Options.Applicative

import qualified Contemplata.Ancor as Ancor
import qualified Contemplata.Ancor.Types as Ancor
import qualified Contemplata.Ancor.IO.Parse as Parse
import qualified Contemplata.Ancor.IO.Show as Show
import qualified Contemplata.Ancor.Preprocess as Pre
import qualified Contemplata.Types as Contemplata
import qualified Contemplata.DB as DB
import qualified Contemplata.WebSocketServer as Server
import qualified Contemplata.Format.Penn as Penn
import qualified Contemplata.Format.FTB as FTB
import qualified Contemplata.Parser.Stanford as Stanford
import qualified Contemplata.Parser.DiscoDOP as DiscoDOP


--------------------------------------------------
-- Commands
--------------------------------------------------


data Command

    = Simplify FilePath
      -- ^ Parse and show the sentences in the Ancor XML file

    | NewDB FilePath
      -- ^ Create a new DB under a given path

    | AddFileDB Bool FilePath FilePath
      -- ^ Add a new file to a given DB

    | RenameFileDB String String FilePath
      -- ^ Rename a file in a given DB

    | CopyFileDB String String FilePath
      -- ^ Copy a file in a given DB

    | StatsDB FilePath
      -- ^ Print statistics related to the DB

    | FTB2Penn FilePath Bool Bool (Maybe String) (Maybe FilePath)
      -- ^ Convert a FTB XML file to the Penn format

    | Penn2JSON
      -- ^ Convert a PTB-style file to JSON

    | Ancor2JSON (Maybe FilePath)
      -- ^ Ancor file -> Contemplata (in JSON); the optional argument specifies the
      -- path to the external file with sentences to be removed

    | GetLabels FilePath
      -- ^ Get the list of (non-terminal) node labels in the DB


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


ancor2jsonOptions :: Parser Command
ancor2jsonOptions = Ancor2JSON
  <$> (optional . strOption)
        ( long "remove"
       <> short 'r'
       <> metavar "FILE"
       <> help "" )


penn2jsonOptions :: Parser Command
penn2jsonOptions = pure Penn2JSON


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
  <*> switch
        ( long "enrich-pos"
       <> short 'p'
       <> help "Enrich POS tags with selected subcategories, as in the default tagset of the Stanford parser" )
  <*> (optional . strOption)
        ( long "root"
       <> help "Root label" )
  <*> (optional . strOption)
        ( long "outdir"
       <> short 'o'
       <> metavar "DIR"
       <> help "Output directory (for more fine-grained output)" )


getLabelsOptions :: Parser Command
getLabelsOptions = GetLabels
  <$> strOption
        ( long "dbdir"
       <> short 'd'
       <> metavar "DIR"
       <> help "DB directory" )


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
  <> command "ancor2json"
    (info (helper <*> ancor2jsonOptions)
      (progDesc "Convert an Ancor file (given on stdin) to JSON (given on stdout)")
    )
  <> command "ftb2penn"
    (info (helper <*> ftb2pennOptions)
      (progDesc "Convert a FTB XML file to the Penn format")
    )
  <> command "penn2json"
    (info (helper <*> penn2jsonOptions)
      (progDesc "Convert a PTB-style file to JSON")
    )
  <> command "labels"
    (info (helper <*> getLabelsOptions)
      (progDesc "Retrieve non-terminal labels")
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

    -- Read the ancor file from stdin and output the resulting
    -- Contemplata file in the JSON format
    Ancor2JSON rmFile -> do
      Ancor.processAncor rmFile T.getContents >>= \case
        Left err -> error (T.unpack err)
        Right file -> LBS.putStr (JSON.encode file)

    -- Read the ancor file from stdin and output the resulting
    -- Contemplata file in the JSON format
    Penn2JSON -> do
      pennForest <- map Penn.parseTree . T.lines <$> T.getContents
      let cont = Penn.convertPennFile pennForest
      LBS.putStr (JSON.encode cont)

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
        let fileIdTxt = T.pack (FilePath.takeBaseName jsonPath)
        fileId <- case Contemplata.decodeFileId fileIdTxt of
          Nothing -> Err.throwE $ "cannote decode fileId: " `T.append` fileIdTxt
          Just x  -> return x
        when (not force) $ DB.hasFile fileId >>= \case
          True -> Err.throwE "file ID already present in DB"
          _ -> return ()
        cs <- liftIO $ LBS.readFile jsonPath
        case JSON.eitherDecode cs of
          Left err -> Err.throwE "JSON decoding failed"
          Right json -> DB.saveFile fileId Contemplata.defaultMeta json
      case res of
        Left err -> T.putStrLn $ "Operation failed: " `T.append` err
        Right _  -> return ()
    RenameFileDB from to dbPath -> do
      let dbConf = DB.defaultConf dbPath
      res <- DB.runDBT dbConf $ do
        fromId <- decodeFileId $ T.pack from
        toId <- decodeFileId $ T.pack to
        DB.renameFile fromId toId
      case res of
        Left err -> T.putStrLn $ "Operation failed: " `T.append` err
        Right _  -> return ()
    CopyFileDB from to dbPath -> do
      let dbConf = DB.defaultConf dbPath
      res <- DB.runDBT dbConf $ do
        fromId <- decodeFileId $ T.pack from
        toId <- decodeFileId $ T.pack to
        DB.copyFile fromId toId
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
            [ Contemplata.encodeFileId fileId
            , " => "
            , T.pack (show $ numberOfLeavesF file) ]
      case res of
        Left err -> T.putStrLn $ "Operation failed: " `T.append` err
        Right _  -> return ()

    -- Misc
    FTB2Penn filePath rmPunc enrichPOS rootLabel outPathMay -> do
      let prepare =
            if rmPunc
            then mapMaybe (\(i, t) -> (,) <$> pure i <*> FTB.rmPunc t)
            else id
          addRoot = case rootLabel of
            Nothing -> id
            Just x  -> R.Node (T.pack x) . pure
          toPenn = addRoot . FTB.toPenn enrichPOS
      pennPairs <- map (Arr.second toPenn)
                . prepare
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

    GetLabels dbPath -> do
      let dbConf = DB.defaultConf dbPath
      res <- DB.runDBT dbConf . flip State.execStateT S.empty $ do
        ids <- S.toList <$> lift DB.fileSet
        forM_ ids $ \fileId -> do
          Contemplata.File{..} <- lift $ DB.loadFile fileId
          forM_ (M.toList treeMap) $ \(_treeId, tree) -> do
            let
              getNodeVal Contemplata.Node{..} = Just nodeVal
              getNodeVal Contemplata.Leaf{..} = Nothing
              labels = mapMaybe getNodeVal (R.flatten tree)
            -- liftIO . T.putStrLn . T.unwords $ labels
            forM_ labels $ \x -> State.modify' (S.insert x)
      case res of
        Left err -> T.putStrLn $ "Operation failed: " `T.append` err
        Right xs  -> forM_ (S.toList xs) T.putStrLn


main :: IO ()
main =
    execParser optsExt >>= run
  where
    optsExt = info (helper <*> opts)
       ( fullDesc
      <> progDesc "Working with Contempata database files"
      <> header "contemplata" )


-------------------------------------------------------
-- Utils
-------------------------------------------------------


-- | Number of leaves in a given file.
numberOfLeavesF :: Contemplata.File -> Int
numberOfLeavesF Contemplata.File{..} = sum
  [ numberOfLeavesT tree
  | (treeId, tree) <- M.toList treeMap ]


-- | Number of leaves in a given tree.
numberOfLeavesT :: Contemplata.Tree -> Int
numberOfLeavesT
  = length
  . filter isLeaf
  . R.flatten
  where
    isLeaf x = case x of
      Contemplata.Leaf{} -> True
      _ -> False


-- | Decode the file ID in the DBT monad.
decodeFileId :: T.Text -> DB.DBT Contemplata.FileId
decodeFileId x =
  let err = Err.throwE $ "cannote decode fileId: " `T.append` x
  in  maybe err return $ Contemplata.decodeFileId x
