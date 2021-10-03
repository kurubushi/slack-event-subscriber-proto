{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot.Slack.Event
  ( -- events
    Event(..)
  , URLVerificationEvent(..)
  , ReactionAddedEvent(..)
    -- items
  , MessageItem(..)
  ) where

import           Bot.Slack.Types   (ChannelID, Emoji, Timestamp, UserID)
import           Data.Aeson        (FromJSON (..), Object,
                                    Value (Object, String), genericParseJSON,
                                    (.:))
import           Data.Aeson.Casing (aesonDrop, snakeCase)
import           Data.Aeson.Types  (Parser)
import           Data.Text         (Text)
import           GHC.Generics      (Generic)

data Event
  = Unknown
  | URLVerification URLVerificationEvent
  | ReactionAdded ReactionAddedEvent

instance FromJSON Event where
  parseJSON o@(Object v) = do
    typ <- v .: "type"
    case typ of
      String "url_verification" -> URLVerification <$> parseJSON o
      String "reaction_added"   -> ReactionAdded <$> parseJSON o
      String t                  -> fail $ "unexpected type: " ++ show t
      _                         -> fail "type is not string"
  parseJSON _ = fail "parsing Coord failed"

-- https://api.slack.com/events/url_verification
data URLVerificationEvent = URLVerificationEvent
  { _urlVerificationEventToken     :: Text
  , _urlVerificationEventChallenge :: Text
  } deriving (Generic)

instance FromJSON URLVerificationEvent where
  parseJSON = genericParseJSON $ aesonDrop (length prefix) snakeCase
    where
      prefix :: String
      prefix = "_urlVerificationEvent"

-- https://api.slack.com/events/reaction_added
data ReactionAddedEvent = ReactionAddedEvent
  { _reactionAddedEventUser     :: UserID
  , _reactionAddedEventReaction :: Emoji
  , _reactionAddedEventItemUser :: UserID
  , _reactionAddedEventItem     :: MessageItem
  , _reactionAddedEventEventTs  :: Timestamp
  } deriving (Generic)

instance FromJSON ReactionAddedEvent where
  parseJSON = genericParseJSON $ aesonDrop (length prefix) snakeCase
    where
      prefix :: String
      prefix = "_reactionAddedEvent"

data MessageItem = MessageItem
  { _messageItemChannel :: ChannelID
  , _messageItemTs      :: Timestamp
  } deriving (Generic)

instance FromJSON MessageItem where
  parseJSON = genericParseJSON $ aesonDrop (length prefix) snakeCase
    where
      prefix :: String
      prefix = "_item"

