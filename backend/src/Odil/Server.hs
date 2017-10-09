{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}


-- | The annotation server.


module Odil.Server
(
-- * Types
  module Odil.Server.Types

-- * Messages
, ParserTyp (..)
, Request (..)
, Answer (..)

-- * Server
, runServer
, loadDB
, application
) where


import GHC.Generics

import Control.Monad (forM_, forever, (<=<))
import Control.Arrow (second)
import qualified Control.Exception as Exc
import qualified Control.Concurrent as C
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Tree as R

import qualified Data.Fixed as Fixed
import qualified Data.Time as Time
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Network.WebSockets as WS

import qualified Data.Aeson as JSON
import Data.Aeson ((.=))

import Odil.Server.Types
import qualified Odil.Server.Config as Cfg
import qualified Odil.Server.DB as DB
import qualified Odil.Stanford as Stanford
import qualified Odil.DiscoDOP as DiscoDOP
import qualified Odil.Penn as Penn


-----------
-- Messages
-----------


-- | The type of parser to use.
data ParserTyp
  = Stanford
  | DiscoDOP
  deriving (Generic, Show)

instance JSON.FromJSON ParserTyp
instance JSON.ToJSON ParserTyp where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

data ParseReq a
  = Single a
  | Batch [a]
  deriving (Generic, Show, Ord, Eq)

instance JSON.FromJSON a => JSON.FromJSON (ParseReq a)
instance JSON.ToJSON a => JSON.ToJSON (ParseReq a) where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

-- | Request coming from the client.
data Request
  = GetFiles
  | GetFile FileId
  | SaveFile FileId File
  | ParseSent FileId TreeId ParserTyp (ParseReq [Stanford.Orth])
    -- ^ FileId and TreeId are sent there and back so that it
    -- can be checked that the user did not move elsewhere before
    -- he/she got the answer for this request
  | ParseSentPos FileId TreeId ParserTyp (ParseReq [(Stanford.Orth, Stanford.Pos)])
    -- ^ Similar to `ParseSent`, but with POS information
  | ParseSentCons FileId TreeId ParserTyp [(Int, Int)] [(Stanford.Orth, Stanford.Pos)]
    -- ^ Similar to `ParseSent`, but with an additional constraint (constituent
    -- with the given pair of positions must exist in the tree)
  | ParseRaw FileId TreeId T.Text
    -- ^ Parse raw sentence
  deriving (Generic, Show)

instance JSON.FromJSON Request
instance JSON.ToJSON Request where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions


-- | Answers for the client.
data Answer
  = Files [FileId]
    -- ^ The list of files
  | NewFile FileId File
    -- ^ New file to edit
  | ParseResult FileId TreeId Tree
    -- ^ Parsing results
  | Notification T.Text
    -- ^ Notication message
  deriving (Show)

instance JSON.ToJSON Answer where
  toJSON x = case x of
    Files xs -> JSON.object
      [ "files" .= xs ]
    NewFile fileId file -> JSON.object
      [ "fileId" .= fileId
      , "file" .= file ]
    ParseResult fileId treeId tree -> JSON.object
      [ "fileId" .= fileId
      , "treeId" .= treeId
      , "tree" .= tree ]
    Notification msg -> JSON.object
      [ "notification" .= msg ]


-----------
-- Model
-----------


-- | Load the DB from a given directory.
loadDB :: FilePath -> IO DB.DB
loadDB dbPath = do
  let db = DB.defaultConf dbPath
  res <- DB.runDBT db DB.createDB
--     mapM_
--       (uncurry DB.saveFile)
--       (M.toList Cfg.tempModel)
  case res of
    Left err -> T.putStrLn $ "Could not create DB: " `T.append` err
    Right _  -> return ()
  return db
  -- Cfg.tempModel


-----------
-- Main
-----------


runServer
  :: FilePath -- ^ DB path
  -> String -- ^ Server address
  -> Int -- ^ Port
  -> IO ()
runServer dbPath serverAddr serverPort = do
  state <- C.newMVar =<< loadDB dbPath
  -- WS.runServer Cfg.serverAddr Cfg.serverPort $ application state
  WS.runServer serverAddr serverPort $ application state


-----------
-- App
-----------


-- | The server application.
application :: C.MVar DB.DB -> WS.ServerApp
application state pending = do
  putStrLn "WS: waiting for request..."
  conn <- WS.acceptRequest pending
  putStrLn "WS: request obtained"
  WS.forkPingThread conn 30
  -- msg <- WS.receiveData conn
  -- clients <- C.readMVar state
  talk conn state


talk :: WS.Connection -> C.MVar DB.DB -> IO ()
talk conn state = forever $ do
  putStrLn $ "WS: init talking"
  msg <- WS.receiveData conn
  putStrLn "WS: obtained message:"
  LBC.putStrLn $ LBC.take 100 msg

  putStrLn "WS: taking DB MVar"
  db <- C.takeMVar state
  putStrLn "WS: DB MVar taken"
  flip Exc.finally (C.putMVar state db >> putStrLn "WS: DB MVar returned") $ do

    DB.runDBT db DB.fileNum >>= \case
      Left err -> do
        T.putStrLn "WS: could not start talking because of:"
        T.putStrLn err
      Right k  -> putStrLn $
        "Talking; the size of the fileMap is " ++ show k

    case JSON.eitherDecode msg of

      Left err -> do
        let msg = T.concat ["JSON decoding error: ", T.pack err]
        T.putStrLn msg
        WS.sendTextData conn . JSON.encode =<< mkNotif msg

      Right GetFiles -> do
        DB.runDBT db DB.fileSet >>= \case
          Left err -> do
            let msg = T.concat ["GetFiles error: ", err]
            T.putStrLn msg
            WS.sendTextData conn . JSON.encode =<< mkNotif msg
          Right fs -> do
            let ret = Files (S.toList fs)
            WS.sendTextData conn (JSON.encode ret)

      Right (GetFile fileId) -> do
        DB.runDBT db (DB.loadFile fileId) >>= \case
          Left err -> do
            let msg = T.concat ["GetFile error: ", err]
            T.putStrLn msg
            WS.sendTextData conn . JSON.encode =<< mkNotif msg
          Right file -> do
            let ret = NewFile fileId file
            WS.sendTextData conn (JSON.encode ret)

      Right (SaveFile fileId file) -> do
        putStrLn "Saving file..."
        DB.runDBT db (DB.saveFile fileId file) >>= \case
          Left err -> do
            let msg = T.concat ["Could not save file: ", err]
            T.putStrLn msg
            WS.sendTextData conn . JSON.encode =<< mkNotif msg
          Right () -> do
            putStrLn "Saved"
            let msg = T.concat ["File ", fileId, " saved"]
            WS.sendTextData conn . JSON.encode =<< mkNotif msg

      Right (ParseRaw fileId treeId txt) -> do
        putStrLn "Parsing raw sentence..."
        treeMay <- Stanford.parseFR txt
        case treeMay of
          Nothing -> do
            let msg = T.concat ["Could not parse: ", txt]
            T.putStrLn msg
            WS.sendTextData conn . JSON.encode =<< mkNotif msg
          Just t -> do
            let ret = ParseResult fileId treeId (Penn.toOdilTree t)
            WS.sendTextData conn (JSON.encode ret)
            let msg = T.concat ["Parsed successfully"]
            T.putStrLn msg
            WS.sendTextData conn . JSON.encode =<< mkNotif msg

      Right (ParseSent fileId treeId parTyp parseReq) -> do
        putStrLn "Parsing tokenized sentence..."
        let parser = case parTyp of
              Stanford -> Stanford.parseTokenizedFR
              DiscoDOP -> DiscoDOP.tagParseDOP Nothing
        treeMay <- case parseReq of
          Single ws -> parser ws
          Batch wss ->
            -- make sure that each sentence in the batch was sucessfully parsed
            -- and join the resulting forest into one tree
            let process = Stanford.joinSentences <=< allJust
            in  process <$> mapM parser wss
        case treeMay of
          Nothing -> do
            let ws = case parseReq of
                       Single x -> x
                       Batch xs -> concat xs
            let msg = T.concat ["Could not parse: ", T.unwords ws]
            T.putStrLn msg
            WS.sendTextData conn . JSON.encode =<< mkNotif msg
          Just t -> do
            let ret = ParseResult fileId treeId (Penn.toOdilTree t)
            WS.sendTextData conn (JSON.encode ret)
            let msg = T.concat ["Parsed successfully"]
            T.putStrLn msg
            WS.sendTextData conn . JSON.encode =<< mkNotif msg

      -- Right (ParseSentPos fileId treeId parTyp ws) -> do
      Right (ParseSentPos fileId treeId parTyp parseReq) -> do
        putStrLn "Parsing tokenized+POSed sentence..."
        let parser = case parTyp of
              Stanford -> Stanford.parsePosFR
              DiscoDOP -> DiscoDOP.parseDOP Nothing
        treeMay <- case parseReq of
          Single ws -> parser ws
          Batch wss ->
            -- make sure that each sentence in the batch was sucessfully parsed
            -- and join the resulting forest into one tree
            let process = Stanford.joinSentences <=< allJust
            in  process <$> mapM parser wss
        case treeMay of
          Nothing -> do
            let ws = case parseReq of
                       Single x -> x
                       Batch xs -> concat xs
                ws' = flip map ws $ \(orth, pos) -> T.concat [orth, ":", pos]
                msg = T.concat ["Could not parse: ", T.unwords ws']
            T.putStrLn msg
            WS.sendTextData conn . JSON.encode =<< mkNotif msg
          Just t -> do
            let ret = ParseResult fileId treeId (Penn.toOdilTree t)
            WS.sendTextData conn (JSON.encode ret)
            let msg = T.concat ["Parsed successfully"]
            T.putStrLn msg
            WS.sendTextData conn . JSON.encode =<< mkNotif msg

      Right (ParseSentCons fileId treeId parTyp cons ws) -> do
        putStrLn $ "Parsing tokenized+POSed sentence with constraints: " ++ show cons
        -- putStrLn $ show cons
        treeMay <- case parTyp of
          Stanford -> Stanford.parseConsFR ws (map (second (+1)) cons)
          DiscoDOP -> DiscoDOP.parseDOP' cons ws
        case treeMay of
          Nothing -> do
            let ws' = flip map ws $ \(orth, pos) -> T.concat [orth, ":", pos]
                msg = T.concat ["Could not parse: ", T.unwords ws']
            T.putStrLn msg
            WS.sendTextData conn . JSON.encode =<< mkNotif msg
          Just t -> do
            let ret = ParseResult fileId treeId (Penn.toOdilTree t)
            WS.sendTextData conn (JSON.encode ret)
            let msg = T.concat ["Parsed successfully"]
            T.putStrLn msg
            WS.sendTextData conn . JSON.encode =<< mkNotif msg


-----------
-- Utils
-----------


-- | Create notification while adding the current time.
mkNotif :: T.Text -> IO Answer
mkNotif msg = do
  time <- Time.getCurrentTime
  zone <- Time.getCurrentTimeZone
  let strTimeRaw
        = Time.formatTime Time.defaultTimeLocale "%X"
        . Time.localTimeOfDay
        $ Time.utcToLocalTime zone time
      strTime = T.pack $ "[" ++ strTimeRaw ++ "] "
      notif = T.append strTime msg
  return $ Notification notif


-- | Assure that all values are Just.
allJust :: [Maybe a] -> Maybe [a]
allJust [] = Just []
allJust (x : xs) = do
  x' <- x
  xs' <- allJust xs
  return $ x' : xs'
