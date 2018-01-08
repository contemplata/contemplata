{-# LANGUAGE OverloadedStrings #-}


-- | Digestive form utils.
--
-- This is mostly a copy of the `Text.Digestive.Snap` module from the
-- `digestive-functors-snap` package. The difference is that the code here
-- allows for several forms on the same page by checking the name of the submit
-- button being pushed.


module Util.Digestive
( runForm
, runFormWith
) where


import           Control.Monad.IO.Class (liftIO)

import System.Directory (copyFile, getTemporaryDirectory)
import System.FilePath (takeFileName, (</>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Maybe as Maybe
import qualified Data.Map.Strict as M

import qualified Data.Text as T
-- import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

import qualified Snap as Snap
import qualified Snap.Util.FileUploads as Snap

import           Text.Digestive.Types (Method(..))
import qualified Text.Digestive.Types as DT
import           Text.Digestive.Form.Encoding (FormEncType(..))
import qualified Text.Digestive.Form as D
import qualified Text.Digestive.View as D
import qualified Text.Digestive.Snap as D


---------------------------------------
-- Digestive form utils
---------------------------------------


snapEnv :: Snap.MonadSnap m => [(T.Text, FilePath)] -> DT.Env m
snapEnv allFiles path = do
    inputs <- map (DT.TextInput . T.decodeUtf8) . findParams <$> Snap.getParams
    let files = map (DT.FileInput . snd) $ filter ((== name) . fst) allFiles
    return $ inputs ++ files
  where
    findParams = Maybe.fromMaybe [] . M.lookup (T.encodeUtf8 name)
    name       = DT.fromPath path


-- | Deals with uploaded files, by placing each file in the temporary directory
-- specified in the configuration. It returns a mapping of names to the
-- temporary files.
snapFiles :: Snap.MonadSnap m => D.SnapFormConfig -> m [(T.Text, FilePath)]
snapFiles config = do
    -- Get the temporary dir or use the one provided by the OS
    tmpDir <- liftIO $ maybe getTemporaryDirectory return $
        D.temporaryDirectory config
    -- Actually do the work...
    fmap Maybe.catMaybes $ Snap.handleFileUploads tmpDir (D.uploadPolicy config) (D.partPolicy config) $
        storeFile tmpDir
  where
    storeFile _   _        (Left _)      = return Nothing
    storeFile tmp partinfo (Right path)  = do
        let newPath = tmp </> "_" ++ takeFileName path ++
                maybe "" BC.unpack (Snap.partFileName partinfo)
        liftIO $ copyFile path newPath
        return $ Just (T.decodeUtf8 $ Snap.partFieldName partinfo, newPath)



-- | Runs a form with the HTTP input provided by Snap.
--
-- Automatically picks between 'getForm' and 'postForm' based on the request
-- method. Set 'method' in the 'SnapFormConfig' to override this behaviour.
runForm :: Snap.MonadSnap m
        => T.Text                 -- ^ Name (id) for the form
        -> BS.ByteString          -- ^ Name of the submit button
        -> D.Form v m a           -- ^ Form to run
        -> m (D.View v, Maybe a)  -- ^ Result
runForm = runFormWith D.defaultSnapFormConfig


-- | Runs a form with a custom upload policy, and HTTP input from snap.
--
-- Automatically picks between 'getForm' and 'postForm' based on request
-- method. Set 'method' in the 'SnapFormConfig' to override this behaviour.
runFormWith
  :: Snap.MonadSnap m
  => D.SnapFormConfig      -- ^ Tempdir and upload policies
  -> T.Text                -- ^ Name (id) for the form
  -> BS.ByteString         -- ^ Name of the submit button
  -> D.Form v m a          -- ^ Form to run
  -> m (D.View v, Maybe a) -- ^ Result
runFormWith config name submitName form = do
  m <- maybe snapMethod return (D.method config)
  case m of
    Get -> do
      view <- D.getForm name form
      return (view, Nothing)
    Post -> do
      -- Get the request and check the value of the button clicked
      submit <- Snap.rqParam submitName <$> Snap.getRequest
      case submit of
        Nothing -> do
          view <- D.getForm name form
          return (view, Nothing)
        Just _ -> do
          D.postForm name form $ \encType -> case encType of
            UrlEncoded -> return $ snapEnv []
            MultiPart  -> snapEnv <$> snapFiles config
  where
    snapMethod        = toMethod . Snap.rqMethod <$> Snap.getRequest
    toMethod Snap.GET = Get
    toMethod _        = Post
