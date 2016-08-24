{-# LANGUAGE DeriveGeneric #-}

module Assets.Store where

import           Import.NoFoundation
import           Data.UUID
import           Data.UUID.V4
import           Data.Conduit.Binary  (sinkLbs)
import           Numeric              (showHex)
import           System.Directory     (createDirectoryIfMissing)
import qualified Data.ByteString.Lazy as LB
import qualified Crypto.Hash.SHA1     as SHA1
import qualified Crypto.Hash.MD5      as MD5

newtype AssetId = AssetId UUID deriving (Generic, Show, Ord, Eq)

newtype AssetFolder = AssetFolder { getFilePath :: FilePath }

type SHA1 = String
type MD5 = String

data Asset = Asset {
             assetId  :: AssetId
           , putOn    :: UTCTime
           , sha1     :: SHA1
           , md5      :: MD5
           , name     :: Text
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
    af   = assetFolder fp assetId
    afn  = assetFileName af assetId

    sha1 = concatMap (flip showHex "") $ SHA1.hashlazy bytes
    md5  = concatMap (flip showHex "") $ MD5.hashlazy bytes
    name = fileName f
    size = LB.length bytes

  _ <- createAssetFolder af
  _ <- fileMove f afn

  return $ Asset{..}

assetFileName :: AssetFolder -> AssetId -> FilePath
assetFileName af (AssetId aId) = (getFilePath af) </> (toString aId)

assetFolder :: FilePath -> AssetId -> AssetFolder
assetFolder fp aId =
  AssetFolder $ fp </> (splitAssetIdIntoSubFolders aId)

  where
    splitAssetIdIntoSubFolders (AssetId u) = case (toString u) of
      (a:b:c:d:_)  -> a : '/' : b : '/' : c : '/' : d : []
      _            -> error "this will never happen"

createAssetFolder :: AssetFolder -> IO ()
createAssetFolder af = createDirectoryIfMissing True $ getFilePath af