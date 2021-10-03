{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot.Slack.API.Conversations.Replies
  ( -- types
    Response(..)
  , Request(..)
  , Message(..)
  , get
  , defaultRequest
  ) where

import           Bot.Slack.Types     (AccessToken, ChannelID, Timestamp, UserID)
import           Data.Aeson          (FromJSON (..), ToJSON (..), (.=))
import qualified Data.Aeson          as A
import qualified Data.Aeson.Casing   as AC
import           Data.Text           (Text)
import           GHC.Generics        (Generic)
import qualified Network.HTTP.Simple as HTTP

-- ref. https://api.slack.com/methods/conversations.replies
endpoint :: String
endpoint = "https://slack.com/api/conversations.replies"

newtype Response = Response
  { _responseMessages :: [Message]
  } deriving (Generic)

instance FromJSON Response where
  parseJSON = A.genericParseJSON $ AC.aesonDrop (length prefix) AC.snakeCase
    where
      prefix :: String
      prefix = "_response"

data Message = Message
  { _messageUser :: UserID
  , _messageText :: Text
  } deriving (Generic)

instance FromJSON Message where
  parseJSON = A.genericParseJSON $ AC.aesonDrop (length prefix) AC.snakeCase
    where
      prefix :: String
      prefix = "_message"

data Request = Request
  { _requestToken     :: AccessToken
  , _requestChannel   :: ChannelID
  , _requestTs        :: Timestamp
  , _requestLimit     :: Int
  , _requestInclusive :: Bool
  } deriving (Generic)

instance ToJSON Request where
  toJSON (Request token channel ts limit inclusive)
    = A.object
        [ "token" .= token
        , "channel" .= channel
        , "ts" .= ts
        , "limit" .= limit
        , "inclusive" .= inclusive
        ]

defaultRequest :: Request
defaultRequest = Request
  { _requestToken = "xxxxx"
  , _requestChannel = "CXXXXX"
  , _requestTs = "1234567890.123456"
  , _requestLimit = 1
  , _requestInclusive = True
  }

get :: Request -> IO Response
get request = do
  req <- HTTP.parseRequest endpoint
  let req' = HTTP.setRequestMethod "GET"
           . HTTP.setRequestBodyJSON request
           $ req
  res <- HTTP.httpJSON req'
  return $ HTTP.getResponseBody res
