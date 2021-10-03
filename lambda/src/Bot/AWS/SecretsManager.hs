module Bot.AWS.SecretsManager
  ( Name
  , Key
  , Value
  , SecretKeys
  , getSecrets
  ) where

import           Control.Lens               ((.~), (^.))
import           Control.Lens.Prism         (_Just)
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy       as BSL
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as TE
import           Network.AWS                (Credentials (Discover),
                                             Region (Tokyo), envRegion, newEnv,
                                             runAWS, runResourceT, send)
import           Network.AWS.SecretsManager (getSecretValue, gsvrsSecretString)

type Name = Text
type Key = Text
type Value = Text
type SecretKeys = Map Key Value

getSecrets :: Name -> IO SecretKeys
getSecrets name = do
  env <- (envRegion .~ Tokyo) <$> newEnv Discover
  res <- runResourceT . runAWS env . send . getSecretValue $ name
  return . fromMaybe M.empty $ do
    json <- res ^. gsvrsSecretString
    Aeson.decode . BSL.fromStrict . TE.encodeUtf8 $ json
