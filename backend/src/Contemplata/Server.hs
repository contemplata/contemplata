{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}


-- | Contemplata's WebSocket server, responsible for handling requests from
-- frontend annotation tool.


module Contemplata.Server
(
-- * Types
  module Contemplata.Types

-- * Messages
, ParserTyp (..)
, Request (..)
, Answer (..)

-- * Server
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

import qualified Dhall as Dhall

import qualified Network.WebSockets as WS

import Contemplata.Types
-- import qualified Contemplata.Server.Config as Cfg
import qualified Contemplata.DB as DB
import qualified Contemplata.Config as AnnoConfig
import qualified Contemplata.Stanford as Stanford
import qualified Contemplata.DiscoDOP as DiscoDOP
import qualified Contemplata.Penn as Penn
import qualified Contemplata.Ancor.Preprocess.Token as Pre


-----------
-- Messages
-----------


-- | Which syntactic parser to use.
data ParserTyp
  = Stanford
  | DiscoDOP
  deriving (Generic, Show)

instance JSON.FromJSON ParserTyp
instance JSON.ToJSON ParserTyp where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

-- | Two types of parsing requests.  OBSOLETE soon.
data ParseReq a
  = Single a
  | Batch [a]
  deriving (Generic, Show, Ord, Eq)

instance JSON.FromJSON a => JSON.FromJSON (ParseReq a)
instance JSON.ToJSON a => JSON.ToJSON (ParseReq a) where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

-- | A request coming from the frontend annotation tool.
data Request
  = GetFile AnnoName FileId
    -- ^ Retrieve a file for the given annotator
  | GetFiles AnnoName [FileId]
    -- ^ Retrieve a list of files for the annotator.  TODO: redundant?
  | GetConfig
    -- ^ A request for annotation configuration
  | SaveFile AnnoName FileId File
    -- ^ Store the file in the database
  | CompareFiles [(FileId, File)]
    -- ^ Compare the given file(s) with the file(s) in the DB to see if any
    -- modifications have been made.
  | ParseSent FileId TreeId ParserTyp
    [(Bool, [(Token, Maybe Stanford.Orth)])]
    -- ^ FileId and TreeId are sent there and back so that one can check that
    -- the user did not move elsewhere before he/she got the answer for this
    -- request. The sentence underlying the request is also sent, which allows
    -- to correctly handle tokenization. The Bool values represent information
    -- whether the corresponding sub-sentences should be parsed or not.
  | ParseSentPos FileId TreeId ParserTyp
    [(Bool, [(Token, Maybe (Stanford.Orth, Stanford.Pos))])]
    -- ^ A version of `ParseSent` in which the POS tags assigned to the
    -- individual tokens should not change.
  | ParseRaw
    FileId -- ^ File ID
    TreeId -- ^ Partition ID
    T.Text -- ^ The sentence
    Bool   -- ^ Perform pre-processing (phatic expressions removal)?
    -- ^ Parse raw sentence, including tokeniziation and POS tagging
  | ParseSentCons FileId TreeId ParserTyp
    (ParseReq ([(T.Text, Int, Int)], [(Token, Maybe (Stanford.Orth, Stanford.Pos))]))
    -- ^ Similar to `ParseSentPos`, but with an additional constraint (constituent
    -- with the given pair of positions must exist in the tree)
--   | Break FileId
--     TreeId   -- ^ Partition ID
--     [T.Text] -- ^ The list of sentences to parse
--     -- ^ Break the given partition into its turn components
  deriving (Generic, Show)

instance JSON.FromJSON Request
instance JSON.ToJSON Request where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions


-- | Answers for the client.
data Answer
  = Files [FileId]
    -- ^ The list of file IDs
  | NewFile FileId File
    -- ^ New file to edit
  | NewFiles [(FileId, File)]
    -- ^ New files to edit (adjudicate)
  | Config AnnoConfig.Config
    -- ^ Annotation configuration
  | ParseResult
    FileId       -- ^ File ID
    TreeId       -- ^ Partition ID
    (Maybe Sent) -- ^ The resulting tokenization, if changed
    Tree         -- ^ The resulting tree
    -- ^ Parsing result. Response to `ParseSent`, `ParseSentPos`,
    -- `ParseSentCons`.
  | ParseResultList
    FileId       -- ^ File ID
    TreeId       -- ^ Partition ID
    [Maybe Tree] -- ^ The resulting maybe forest
    -- ^ Parsing result list. Response to `ParseSent`, `ParseSentPos`,
    -- `ParseSentCons`.
    --
    -- WARNING: the trees in the result are not guaranteed to have different
    -- node IDs. More precisely, their node ID sets are not guaranteed to be
    -- disjoint. This is because some subtrees are unknown (`Nothing`) and there
    -- is no way to take their node IDs into account anyway.
  | DiffFiles [FileId]
    -- ^ The IDs of the files which differ from their version in the database.
    -- A response to `CompareFiles`
  | Notification T.Text
    -- ^ Notication message
  deriving (Show)

-- We create the instance manually so as to facilitate writing the JSON parser
-- on the front-end side.
instance JSON.ToJSON Answer where
  toJSON x = case x of
    Files xs -> JSON.object
      [ "files" .= xs ]
    NewFile fileId file -> JSON.object
      [ "tag" .= ("NewFile" :: T.Text)
      , "fileId" .= fileId
      , "file" .= file ]
    NewFiles fileList -> JSON.object
      [ "tag" .= ("NewFiles" :: T.Text)
      , "files" .= JSON.toJSON (map encodePair fileList) ]
      where
        encodePair (fileId, file) = JSON.object
          [ "fileId" .= fileId
          , "file" .= file ]
    Config annoCfg -> JSON.object
      [ "tag" .= ("Config" :: T.Text)
      , "config" .= JSON.toJSON annoCfg ]
      where
        encodePair (fileId, file) = JSON.object
          [ "fileId" .= fileId
          , "file" .= file ]
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
    DiffFiles fileIds -> JSON.object
      [ "fileIds" .= fileIds ]


-----------
-- App
-----------


-- | The WebSocket server application.
application
  :: C.MVar DB.DB
     -- ^ An MVar with the database configuration. We use an MVar so as to
     -- handle WebSocket requests atomically (i.e., as atomic transactions).
  -> Cfg.Config
     -- ^ Web-server's top-level configuration (not to be confused with
     -- annotation configuration)
  -> WS.ServerApp
application state snapCfg pending = do
  putStrLn "WS: waiting for request..."
  conn <- WS.acceptRequest pending
  putStrLn "WS: request obtained"
  WS.forkPingThread conn 30
  talk conn state snapCfg


-- | Handle a WebSocket request from a client.
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

      Right (GetFiles anno fileList) -> do
        let getFile fid = do
              Just _ <- DB.accessLevel fid anno
              file <- DB.loadFile fid
              return (fid, file)
            -- getBoth = (,) <$> getFile fileId <*> getFile compId
            getAll = mapM getFile fileList
        DB.runDBT db getAll >>= \case
          Left err -> do
            let msg = T.concat ["GetFiles error: ", err]
            T.putStrLn msg
            WS.sendTextData conn . JSON.encode =<< mkNotif msg
          Right fileList -> do
            -- let ret = NewFiles [(fileId, file1), (compId, file2)]
            let ret = NewFiles fileList
            WS.sendTextData conn (JSON.encode ret)

      Right GetConfig -> do
        putStrLn "Sending annotation configuration..."
        Just cfgPath <- liftIO $ Cfg.lookup snapCfg "anno-config"
        annoCfg <- liftIO $ Dhall.input Dhall.auto cfgPath
        let ret = Config annoCfg
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

      Right (CompareFiles fileList) -> do
        T.putStrLn $ "Comparing files: " `T.append`
          T.intercalate ", " (map (encodeFileId.fst) fileList)
        DB.runDBT db (mapM DB.loadFile $ map fst fileList) >>= \case
          Left err -> do
            let msg = T.concat ["Could not compare file: ", err]
            T.putStrLn msg
            WS.sendTextData conn . JSON.encode =<< mkNotif msg
          Right files' -> do
            let diff =
                  [ fileId
                  | ((fileId, file), file') <- zip fileList files'
                  , file /= file' ]
                res = DiffFiles diff
            WS.sendTextData conn (JSON.encode res)

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

--       Right (Break fileId partId txts) -> do
--         putStrLn "Breaking the given partition..."
--         let msg = T.unlines txts
--         T.putStrLn msg
--         WS.sendTextData conn . JSON.encode =<< mkNotif msg

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
        let oldTokens = map snd wss
            oldTrees = map (fmap removeRoot) forest
              where removeRoot t = case R.subForest t of
                      [subTree] -> subTree
                      _ -> t
            newForest = Penn.toContemplataForest oldTrees oldTokens
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
            newForest = Penn.toContemplataForest oldTrees oldTokens
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
                (_, tree) = Penn.toContemplataTree' t (simplify sent)
                ret = ParseResult fileId treeId Nothing tree
            WS.sendTextData conn (JSON.encode ret)
            let msg = T.concat ["Parsed successfully"]
            T.putStrLn msg
            WS.sendTextData conn . JSON.encode =<< mkNotif msg


-----------
-- Utils
-----------


-- | Create notification text, adding the current time.
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
dummyTree :: Penn.Tree
dummyTree =
  R.Node "ROOT" [R.Node "" []]


-- | Create a trivial tree from a list of words and their POS tags.
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
        odil = Penn.toContemplataTree dummyTree
    return $ Just (sent, odil)
  else do
    liftIO $ parseSentWith Stanford.parseFR prepSent >>= \case
      Just x -> return (Just x)
      Nothing -> parseSentWith (fmap (fmap simpleTree) . Stanford.posTagFR) prepSent >>= \case
        Just x -> return (Just x)
        Nothing -> return Nothing


-- | Parse the given tokenized sentence using the given raw text parsing
-- function.
parseSentWith
  :: (T.Text -> IO (Maybe Penn.Tree))
     -- ^ The parsing function
  -> [(Token, Maybe T.Text)]
     -- ^ The sentence to parse, together with the pre-processing result
  -> IO (Maybe (Sent, Tree))
parseSentWith parseFun sentPrep = do
  runMaybeT $ do
    let txt = T.intercalate " " . catMaybes . map snd $ sentPrep
    penn <- MaybeT (parseFun txt)
    return $ Penn.toContemplataTree' penn sentPrep
