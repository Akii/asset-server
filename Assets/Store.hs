{-# LANGUAGE DeriveGeneric #-}

module Assets.Store where

import Import
import Data.Conduit.Binary (sinkLbs)
import qualified Data.ByteString.Lazy as LB

import Data.UUID
import Data.UUID.V4
import System.Directory (createDirectoryIfMissing)
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Crypto.Hash.MD5 as MD5

newtype AssetId = AssetId UUID deriving (Generic, Show, Eq)

newtype AssetFolder = AssetFolder { getFilePath :: FilePath }

type SHA1 = Text
type MD5 = Text
type ImportError = Text

data Asset = Asset {
             assetId  :: AssetId
           , putOn    :: UTCTime
           , sha1     :: SHA1
           , md5      :: MD5
--           , name     :: Text
           , size     :: Int64
           } deriving (Generic, Show)

instance ToJSON AssetId where
  toJSON (AssetId uuid) = toJSON (toString uuid)

instance ToJSON Asset

importAsset :: FileInfo -> FilePath -> IO Asset
importAsset f fp = do
  assetId <- AssetId <$> nextRandom
  putOn   <- getCurrentTime
  bytes   <- runResourceT $ fileSource f $$ sinkLbs

  let
    folder = assetFolder fp assetId
    fileName = assetFileName folder assetId

    sha1 = decodeUtf8 $ SHA1.hashlazy bytes
    md5  = decodeUtf8 $ MD5.hashlazy bytes
--    name = fileName f
    size = LB.length bytes

  return $ Asset{..}


assetFileName :: AssetFolder -> AssetId -> FilePath
assetFileName af (AssetId aId) = (getFilePath af) </> (toString aId)

assetFolder :: FilePath -> AssetId -> AssetFolder
assetFolder fp aId =
  AssetFolder $ fp </> (splitAssetIdIntoSubFolders aId)

  where
    splitAssetIdIntoSubFolders (AssetId u) = case (toString u) of
      (a:b:c:d:_)  -> a : '/' : b : '/' : c : '/' : d : []
      _             -> error "this will never happen"

createAssetFolder :: AssetFolder -> IO ()
createAssetFolder af = createDirectoryIfMissing True $ getFilePath af