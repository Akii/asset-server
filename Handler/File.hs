module Handler.File where

import Import
import Data.Aeson (decode)

import Assets.Store

postFileR :: Handler Value
postFileR = do
  result <- runInputPost $ iopt fileField "file"
  case result of
    Just x -> do
      _ <- importAsset x
      sendResponseStatus status201 ()
    Nothing -> sendResponseStatus status400 ()