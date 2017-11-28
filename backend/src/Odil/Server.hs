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

-- * Utils
, parseRetokFR
) where


import GHC.Generics

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Maybe (runMaybeT, MaybeT(..))
import           Control.Monad (forM_, forever, (<=<), when)
import           Control.Arrow (second)
import qualified Control.Exception as Exc
import qualified Control.Concurrent as C
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Tree as R

import           Data.Maybe (listToMaybe, maybeToList, catMaybes, mapMaybe, isNothing)
import qualified Data.Fixed as Fixed
-- import qualified Data.Foldable as F
import qualified Data.Time as Time
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.Configurator as Cfg
import qualified Data.Configurator.Types as Cfg

import qualified Data.Aeson as JSON
import Data.Aeson ((.=))

import qualified Network.WebSockets as WS

import Odil.Server.Types
-- import qualified Odil.Server.Config as Cfg
import qualified Odil.Server.DB as DB
import qualified Odil.Stanford as Stanford
import qualified Odil.DiscoDOP as DiscoDOP
import qualified Odil.Penn as Penn
import qualified Odil.Ancor.Preprocess.Token as Pre


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
  = GetFiles AnnoName
  | GetFile AnnoName FileId
  | GetFile2 AnnoName FileId FileId
  | SaveFile AnnoName FileId File
  | ParseSent FileId TreeId ParserTyp
    [(Bool, [(Token, Maybe Stanford.Orth)])]
    -- ^ FileId and TreeId are sent there and back so that it can be checked
    -- that the user did not move elsewhere before he/she got the answer for
    -- this request. The sentence underlying the request is also sent, which
    -- allows to correctly handle tokenization.
  | ParseSentPos FileId TreeId ParserTyp
    [(Bool, [(Token, Maybe (Stanford.Orth, Stanford.Pos))])]
    -- ^ A version of `ParseSentPos` in which some of the sub-sentences are
    -- not required to be parsed.  We also don't use `ParseReq` anymore.
  -- | ParseSentCons FileId TreeId ParserTyp [(Int, Int)] [(Stanford.Orth, Stanford.Pos)]
  | ParseRaw FileId
    TreeId -- ^ Partition ID
    T.Text -- ^ The sentence
    Bool   -- ^ Perform pre-processing?
    -- ^ Parse raw sentence
  | ParseSentCons FileId TreeId ParserTyp
    (ParseReq ([(T.Text, Int, Int)], [(Token, Maybe (Stanford.Orth, Stanford.Pos))]))
    -- ^ Similar to `ParseSentPos`, but with an additional constraint (constituent
    -- with the given pair of positions must exist in the tree)
  | Break FileId
    TreeId   -- ^ Partition ID
    [T.Text] -- ^ The list of sentences to parse
    -- ^ Break the given partition into its turn components
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
  | NewFile2 FileId File FileId File
    -- ^ New files to adjudicate
  | ParseResult
    FileId
    TreeId
    (Maybe Sent) -- ^ The resulting tokenization, if changed
    Tree -- ^ The resulting tree
    -- ^ Parsing results
  | ParseResultList
    FileId
    TreeId
    [Maybe Tree] -- ^ The resulting maybe forest
    -- ^ Parsing result (list)
    --
    -- WARNING: the trees in the result are not guaranteed to have different
    -- node IDs. This is because some subtrees are unknown (`Nothing`) and there
    -- is no way to take their node IDs into account anyway.
  | Notification T.Text
    -- ^ Notication message
  deriving (Show)

instance JSON.ToJSON Answer where
  toJSON x = case x of
    Files xs -> JSON.object
      [ "files" .= xs ]
    NewFile fileId file -> JSON.object
      [ "tag" .= ("NewFile" :: T.Text)
      , "fileId" .= fileId
      , "file" .= file ]
    NewFile2 fileId file compId comp -> JSON.object
      [ "tag" .= ("NewFile2" :: T.Text)
      , "fileId" .= fileId
      , "file" .= file
      , "compId" .= compId
      , "comp" .= comp ]
    ParseResult fileId treeId sentMay tree -> JSON.object
      [ "fileId" .= fileId
      , "treeId" .= treeId
      , "sent" .= sentMay
      , "tree" .= tree ]
    ParseResultList fileId treeId forest -> JSON.object
      [ "fileId" .= fileId
      , "treeId" .= treeId
      , "forest" .= forest ]
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
  case res of
    Left err -> T.putStrLn $ "Could not create DB: " `T.append` err
    Right _  -> return ()
  return db


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
  WS.runServer serverAddr serverPort $ application state Cfg.empty


-----------
-- App
-----------


-- | The server application.
application :: C.MVar DB.DB -> Cfg.Config -> WS.ServerApp
application state snapCfg pending = do
  putStrLn "WS: waiting for request..."
  conn <- WS.acceptRequest pending
  putStrLn "WS: request obtained"
  WS.forkPingThread conn 30
  -- msg <- WS.receiveData conn
  -- clients <- C.readMVar state
  talk conn state snapCfg


talk :: WS.Connection -> C.MVar DB.DB -> Cfg.Config -> IO ()
talk conn state snapCfg = forever $ do
  putStrLn $ "WS: init talking"
  msg <- WS.receiveData conn
  putStrLn "WS: obtained message:"
  LBC.putStrLn $ LBC.take 250 msg `LBS.append` "..."

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

      -- TODO: What's the point of sending the annotator name? This should be
      -- immediately accessible at the server side!
      Right (GetFiles anno) -> do
        DB.runDBT db (DB.fileSetFor anno $ const True) >>= \case
        -- DB.runDBT db DB.fileSet >>= \case
          Left err -> do
            let msg = T.concat ["GetFiles error: ", err]
            T.putStrLn msg
            WS.sendTextData conn . JSON.encode =<< mkNotif msg
          Right fs -> do
            let ret = Files (S.toList fs)
            WS.sendTextData conn (JSON.encode ret)

      Right (GetFile anno fileId) -> do
        let getFile = do
              Just _ <- DB.accessLevel fileId anno
              DB.loadFile fileId
        DB.runDBT db getFile >>= \case
          Left err -> do
            let msg = T.concat ["GetFile error: ", err]
            T.putStrLn msg
            WS.sendTextData conn . JSON.encode =<< mkNotif msg
          Right file -> do
            let ret = NewFile fileId file
            WS.sendTextData conn (JSON.encode ret)

      Right (GetFile2 anno fileId compId) -> do
        -- putStrLn $ "Running GetFile2 for " ++ show (fileId, compId)
        let getFile fid = do
              Just _ <- DB.accessLevel fid anno
              DB.loadFile fid
            getBoth = (,) <$> getFile fileId <*> getFile compId
        DB.runDBT db getBoth >>= \case
          Left err -> do
            let msg = T.concat ["GetFile2 error: ", err]
            T.putStrLn msg
            WS.sendTextData conn . JSON.encode =<< mkNotif msg
          Right (file1, file2) -> do
            let ret = NewFile2 fileId file1 compId file2
            WS.sendTextData conn (JSON.encode ret)

      Right (SaveFile anno fileId file) -> do
        putStrLn "Saving file..."
        let saveFile = do
              Just accLevel <- DB.accessLevel fileId anno
              if accLevel < Write
                then fail "you are not authorized to modify this file"
                else return ()
              status <- fileStatus <$> DB.loadMeta fileId
              if status == Done
                then fail "annotation of the file is already finished"
                else return ()
              DB.reSaveFile fileId file
        DB.runDBT db saveFile >>= \case
          Left err -> do
            let msg = T.concat ["Could not save file: ", err]
            T.putStrLn msg
            WS.sendTextData conn . JSON.encode =<< mkNotif msg
          Right () -> do
            putStrLn "Saved"
            let msg = T.concat ["File ", encodeFileId fileId, " saved"]
            WS.sendTextData conn . JSON.encode =<< mkNotif msg

      Right (ParseRaw fileId treeId txt0 prep) -> do
        prepare <-
          if not prep
          then do
            putStrLn "Parsing raw sentence..."
            return Pre.prepareDummy
          else do
           putStrLn "Parsing preprocessed sentence..."
           Just rmPath <- liftIO $ Cfg.lookup snapCfg "remove"
           extCfg <- liftIO $ Pre.readConfig rmPath
           return $ Pre.prepare extCfg
        resultMay <- parseRetokFR . prepare . sentFromText $ txt0
        case resultMay of
          Nothing -> do
            let msg = T.concat ["Could not parse: ", txt0]
            T.putStrLn msg
            WS.sendTextData conn . JSON.encode =<< mkNotif msg
          Just (sent, tree) -> do
            let ret = ParseResult fileId treeId (Just sent) tree
            WS.sendTextData conn (JSON.encode ret)
            let msg = T.concat ["Parsed successfully"]
            T.putStrLn msg
            WS.sendTextData conn . JSON.encode =<< mkNotif msg

--       Right (ParseRaw fileId treeId txt0 prep) -> do
--
--         txt <-
--           if not prep
--           then do
--             putStrLn "Parsing raw sentence..."
--             return txt0
--           else do
--             putStrLn "Parsing preprocessed sentence..."
--             Just rmPath <- liftIO $ Cfg.lookup snapCfg "remove"
--             extCfg <- liftIO $ Pre.readConfig rmPath
--             return $ Pre.prepare extCfg txt0
--
--         treeMay <- Stanford.parseFR txt
--         case treeMay of
--           Nothing -> do
--             let msg = T.concat ["Could not parse: ", txt]
--             T.putStrLn msg
--             WS.sendTextData conn . JSON.encode =<< mkNotif msg
--           Just t -> do
--             let ret = ParseResult fileId treeId (Penn.toOdilTree t)
--             WS.sendTextData conn (JSON.encode ret)
--             let msg = T.concat ["Parsed successfully"]
--             T.putStrLn msg
--             WS.sendTextData conn . JSON.encode =<< mkNotif msg

      Right (Break fileId partId txts) -> do
        putStrLn "Breaking the given partition..."
        let msg = T.unlines txts
        T.putStrLn msg
        WS.sendTextData conn . JSON.encode =<< mkNotif msg

--         treeMay <- Stanford.parseFR txt
--         case treeMay of
--           Nothing -> do
--             let msg = T.concat ["Could not parse: ", txt]
--             T.putStrLn msg
--             WS.sendTextData conn . JSON.encode =<< mkNotif msg
--           Just t -> do
--             let ret = ParseResult fileId treeId (Penn.toOdilTree t)
--             WS.sendTextData conn (JSON.encode ret)
--             let msg = T.concat ["Parsed successfully"]
--             T.putStrLn msg
--             WS.sendTextData conn . JSON.encode =<< mkNotif msg

--       Right (ParseSent fileId treeId parTyp parseReq) -> do
--         putStrLn "Parsing tokenized sentence..."
--         let parser = case parTyp of
--               Stanford -> Stanford.parseTokenizedFR . mapMaybe snd
--               DiscoDOP -> DiscoDOP.tagParseDOP Nothing . mapMaybe snd
--             sent = case parseReq of
--               Single ws -> ws
--               Batch wss -> concat wss
--         treeMay <- case parseReq of
--           Single ws -> parser ws
--           Batch wss ->
--             -- make sure that each sentence in the batch was sucessfully parsed
--             -- and join the resulting forest into one tree
--             let process = Stanford.joinSentences <=< allJust
--             in  process <$> mapM parser wss
--         case treeMay of
--           Nothing -> do
--             let ws = mapMaybe snd sent
--                 msg = T.concat ["Could not parse: ", T.unwords ws]
--             T.putStrLn msg
--             WS.sendTextData conn . JSON.encode =<< mkNotif msg
--           Just t -> do
--             -- TODO: should we sent the sentence back, perhaps for additional safety?
--             -- TODO: even more importantly, do we really need to know what are
--             -- the frontiers between the individual sub-sentences? Firstly, we
--             -- seem not to rely on this information. Secondly, at the moment, it
--             -- does not necessarily correspond 100% to the division at the
--             -- front-end side (see the `syncForestWithSent` function in the
--             -- Edit.Model.elm file).
--             --
--             -- Note also that, in general, the frontiers in the tree can differ
--             -- from those stemming from the division into speech turns.
--             let (_, tree) = Penn.toOdilTree' t sent
--                 ret = ParseResult fileId treeId Nothing tree
--             WS.sendTextData conn (JSON.encode ret)
--             let msg = T.concat ["Parsed successfully"]
--             T.putStrLn msg
--             WS.sendTextData conn . JSON.encode =<< mkNotif msg

      Right (ParseSent fileId treeId parTyp wss) -> do
        putStrLn "Parsing tokenized sentence..."
        let parseCore = case parTyp of
              Stanford -> Stanford.parseTokenizedFR . mapMaybe snd
              DiscoDOP -> DiscoDOP.tagParseDOP Nothing . mapMaybe snd
            parser (parseIt, ws) =
              if parseIt
              then parseCore ws
              else return Nothing
        forest <- mapM parser wss

        -- Send what you were able to parse
        let oldForest = zip forest $ map snd wss
            oldTokens = map snd oldForest
            oldTrees = map (fmap removeRoot . fst) oldForest
              where removeRoot t = case R.subForest t of
                      [subTree] -> subTree
                      _ -> t
            newForest = Penn.toOdilForest oldTrees oldTokens
            ret = ParseResultList fileId treeId newForest
        WS.sendTextData conn (JSON.encode ret)

        -- Send a message if something went wrong or not
        if ( length (filter isNothing forest) >
             length (filter ((==False) . fst) wss) )
          then do
            let ws = mapMaybe snd $ concatMap snd wss
                msg = T.concat ["Could not parse some parts of: ", T.unwords ws]
            T.putStrLn msg
            WS.sendTextData conn . JSON.encode =<< mkNotif msg
          else do
            let msg = T.concat ["Parsed successfully"]
            T.putStrLn msg
            WS.sendTextData conn . JSON.encode =<< mkNotif msg

      Right (ParseSentPos fileId treeId parTyp wss) -> do
        putStrLn "Parsing tokenized+POSed sentence..."
        let parseCore = case parTyp of
              Stanford -> Stanford.parsePosFR . mapMaybe snd
              DiscoDOP -> DiscoDOP.parseDOP Nothing . mapMaybe snd
            parser (parseIt, ws) =
              if parseIt
              then parseCore ws
              else return Nothing
        forest <- mapM parser wss

        -- Send what you were able to parse
        let simplify = map . map $ second (fmap fst)
            oldForest = zip forest . simplify $ map snd wss
            oldTokens = map snd oldForest
            oldTrees = map (fmap removeRoot . fst) oldForest
              where removeRoot t = case R.subForest t of
                      [subTree] -> subTree
                      _ -> t
            newForest = Penn.toOdilForest oldTrees oldTokens
            ret = ParseResultList fileId treeId newForest
        WS.sendTextData conn (JSON.encode ret)

        -- Send a message if something went wrong or not
        if ( length (filter isNothing forest) >
             length (filter ((==False) . fst) wss) )
          then do
            let ws = mapMaybe snd $ concatMap snd wss
                ws' = flip map ws $ \(orth, pos) -> T.concat [orth, ":", pos]
                msg = T.concat ["Could not parse some parts of: ", T.unwords ws']
            T.putStrLn msg
            WS.sendTextData conn . JSON.encode =<< mkNotif msg
          else do
            let msg = T.concat ["Parsed successfully"]
            T.putStrLn msg
            WS.sendTextData conn . JSON.encode =<< mkNotif msg



      -- Right (ParseSentCons fileId treeId parTyp cons ws) -> do
      Right (ParseSentCons fileId treeId parTyp parseReq) -> do
        putStrLn $ "Parsing tokenized+POSed sentence with constraints..."
        let parser (cons, ws) = case parTyp of
              Stanford -> Stanford.parseConsFR
                (mapMaybe snd ws)
                (map (second (+1) . rmLabel) cons)
                where
                  rmLabel (_, p, q) = (p, q)
              DiscoDOP -> listToMaybe <$> DiscoDOP.newParseDOP cons
                (mapMaybe snd ws)
            sent = case parseReq of
              Single (cons, ws) -> ws
              Batch wss -> concatMap snd wss
        treeMay <- case parseReq of
          Single (cons, ws) -> parser (cons, ws)
          Batch wss ->
            -- make sure that each sentence in the batch was sucessfully parsed
            -- and join the resulting forest into one tree
            let process = Stanford.joinSentences <=< allJust
            in  process <$> mapM parser wss
        case treeMay of
          Nothing -> do
            let ws = mapMaybe snd sent
                ws' = flip map ws $ \(orth, pos) -> T.concat [orth, ":", pos]
                msg = T.concat ["Could not parse: ", T.unwords ws']
            T.putStrLn msg
            WS.sendTextData conn . JSON.encode =<< mkNotif msg
          Just t -> do
            let simplify = map $ second (fmap fst)
                (_, tree) = Penn.toOdilTree' t (simplify sent)
                ret = ParseResult fileId treeId Nothing tree
            WS.sendTextData conn (JSON.encode ret)
            let msg = T.concat ["Parsed successfully"]
            T.putStrLn msg
            WS.sendTextData conn . JSON.encode =<< mkNotif msg


--       Right (ParseSentCons fileId treeId parTyp cons ws) -> do
--         putStrLn $ "Parsing tokenized+POSed sentence with constraints: " ++ show cons
--         -- putStrLn $ show cons
--         treeMay <- case parTyp of
--           Stanford -> Stanford.parseConsFR ws (map (second (+1)) cons)
--           DiscoDOP -> DiscoDOP.parseDOP' cons ws
--         case treeMay of
--           Nothing -> do
--             let ws' = flip map ws $ \(orth, pos) -> T.concat [orth, ":", pos]
--                 msg = T.concat ["Could not parse: ", T.unwords ws']
--             T.putStrLn msg
--             WS.sendTextData conn . JSON.encode =<< mkNotif msg
--           Just t -> do
--             let ret = ParseResult fileId treeId Nothing (Penn.toOdilTree t)
--             WS.sendTextData conn (JSON.encode ret)
--             let msg = T.concat ["Parsed successfully"]
--             T.putStrLn msg
--             WS.sendTextData conn . JSON.encode =<< mkNotif msg


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


-- | Create a trivial tree from a list of words and their POS tags.
-- | TODO: move to a higher-level module (there is a copy in `Server.hs`).
dummyTree :: Penn.Tree
dummyTree =
  R.Node "ROOT" [R.Node "" []]


-- | Create a trivial tree from a list of words and their POS tags.
-- | TODO: move to a higher-level module (there is a copy in `app/Main.hs`).
simpleTree :: [(Stanford.Orth, Stanford.Pos)] -> Penn.Tree
simpleTree sent =
  R.Node "ROOT" [R.Node "SENT" $ map mkLeaf sent]
  where
    mkLeaf (orth, pos) = R.Node pos [R.Node orth []]


----------------
-- Parsing utils
----------------


-- | Parse the given list of tokens. The `snd` elements of the input lists are
-- `Nothing` if the corresponding tokens should be ignored. Note that the
-- function can change the input tokenization (although it is supposed to
-- guarantee that the resulting tokenization amounts to the same sentence).
parseRetokFR
  :: [(Token, Maybe T.Text)]
  -> IO (Maybe (Sent, Tree))
parseRetokFR prepSent =
  if all ((==Nothing) . snd) prepSent then do
    let sent = [mconcat (map fst prepSent)]
        odil = Penn.toOdilTree dummyTree
    return $ Just (sent, odil)
  else do
    liftIO $ parseTextWith Stanford.parseFR prepSent >>= \case
      Just x -> return (Just x)
      Nothing -> parseTextWith (fmap (fmap simpleTree) . Stanford.posTagFR) prepSent >>= \case
        Just x -> return (Just x)
        Nothing -> return Nothing


parseTextWith
  :: (T.Text -> IO (Maybe Penn.Tree))
     -- ^ The parsing function
  -> [(Token, Maybe T.Text)]
     -- ^ The sentence to parse, together with the pre-processing result
  -> IO (Maybe (Sent, Tree))
parseTextWith parseFun sentPrep = do
  runMaybeT $ do
    let txt = T.intercalate " " . catMaybes . map snd $ sentPrep
    penn <- MaybeT (parseFun txt)
    return $ Penn.toOdilTree' penn sentPrep
