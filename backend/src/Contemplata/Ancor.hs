{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}


-- | Top-level ANCOR processing module.


module Contemplata.Ancor
( processAncor
) where


import           Control.Monad (forM)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as State

import qualified Control.Error as Err
import qualified Control.Exception as Exc

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Contemplata.WebSocketServer as Server
import           Contemplata.Types

import qualified Contemplata.Ancor.Types as Ancor
import qualified Contemplata.Ancor.IO.Parse as Parse
import qualified Contemplata.Ancor.IO.Show as Show
import qualified Contemplata.Ancor.Preprocess as Pre


-- | Process the given ancor file.
processAncor
  :: Maybe FilePath  -- ^ File with the expressions to remove
  -> IO T.Text       -- ^ An IO action which provides the contents of the ANCOR
                     --   file to convert (e.g. `T.getContents`, `T.readFile`)
  -> IO (Either T.Text File)
processAncor rmFile ioAncor = fmap finalize . Exc.try . Err.runExceptT $ do
  ancor <- Parse.parseTrans <$> liftIO ioAncor
  (turns2, treeMap) <- flip State.runStateT M.empty $ do
    forM ancor $ \section -> do
      forM section $ \turn -> do
        treeList <- forM (Ancor.elems turn) $ \(mayWho, elem) -> do
          prepare <- Pre.prepare <$> case rmFile of
            Nothing -> pure []
            Just path -> liftIO $ Pre.readConfig path
          -- let sent = Show.showElem elem
          let sent0 = Show.elem2sent elem
              prepSent = prepare sent0
          (sent, odil) <- liftIO (Server.parseRetokFR prepSent) >>= \case
            Nothing -> lift $
              Err.throwE "Tokenization with Stanford failed"
            Just x -> return x
          k <- State.gets $ (+1) . M.size
          State.modify' $ M.insert k (sent, odil)
          return (k, fmap Ancor.unWho mayWho)
        return $ Turn
          { speaker = Ancor.speaker turn
          , trees = M.fromList treeList }
  return $ mkNewFile treeMap (concat turns2)
  -- LBS.putStr (JSON.encode file)
  where
    finalize x =
      case x of
        Left err -> Left . displayException $ err
        Right errVal -> errVal -- error or value
    displayException :: Exc.SomeException -> T.Text
    displayException = T.pack . Exc.displayException
