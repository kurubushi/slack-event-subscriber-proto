module Bot.AWS.S3
  ( Bucket
  , Key
  , put
  , get
  ) where

import           Control.Lens            ((.~), (^.))
import           Data.Aeson              (FromJSON, ToJSON)
import qualified Data.Aeson              as A
import qualified Data.ByteString.Lazy    as BSL
import qualified Data.Conduit.Attoparsec as CA
import           Data.Text               (Text)
import qualified Data.Text.Encoding      as TE
import           Network.AWS             (Credentials (Discover), Env,
                                          Region (Tokyo), envRegion, newEnv,
                                          runAWS, runResourceT, send, sinkBody,
                                          toBody)
import           Network.AWS.S3          (BucketName (..), ObjectKey (..),
                                          getObject, gorsBody, putObject)

type Bucket = Text
type Key = Text

put :: ToJSON a => Bucket -> Key -> a -> IO ()
put bucket key obj = do
  env <- getEnv
  runResourceT . runAWS env $ do -- Network.AWS.AWS ()
    send $ putObject (BucketName bucket) (ObjectKey key) (fromObj obj)
    return ()
  where
    fromObj = toBody . A.encode

get :: FromJSON a => Bucket -> Key -> IO (Maybe a)
get bucket key = do
  env <- getEnv
  runResourceT . runAWS env $ do -- Network.AWS.AWS (Maybe a)
    res <- send $ getObject (BucketName bucket) (ObjectKey key)
    sinkBody (res ^. gorsBody) parserSink
  where
    parserSink = do -- ConduitT ByteString Void Network.AWS.AWS (Maybe a)
      val <- CA.sinkParser A.json
      return $ case A.fromJSON val of
        A.Success x -> Just x
        A.Error _   -> Nothing

getEnv :: IO Env
getEnv = (envRegion .~ Tokyo) <$> newEnv Discover
