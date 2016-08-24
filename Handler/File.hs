module Handler.File where

import Import
import System.Directory (getCurrentDirectory)
import qualified Data.Map as M

import Assets.Store

getFileR :: Handler Value
getFileR = do
  assetsVar <- appAssets <$> getYesod
  assets <- liftIO $ readTVarIO assetsVar

  sendResponseStatus status200 $ toJSON $ toList assets

postFileR :: Handler Value
postFileR = do
  assetsVar <- appAssets <$> getYesod
  result      <- runInputPost $ iopt fileField "file"
  importPath  <- appAssetLocation <$> appSettings <$> getYesod
  currDir     <- liftIO $ getCurrentDirectory

  case result of
    Just fi -> do
      a <- liftIO $ importAsset fi $ currDir </> importPath
      liftIO . atomically $ do
              modifyTVar assetsVar $ \as -> M.insert (assetId a) a as

      sendResponseStatus status201 $ toJSON a
    Nothing -> sendResponseStatus status400 ()