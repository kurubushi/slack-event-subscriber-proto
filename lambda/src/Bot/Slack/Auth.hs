{-# LANGUAGE OverloadedStrings #-}

module Bot.Slack.Auth
  ( SigningSecret
  , Timestamp
  , Signature
  , Body
  , verify
  ) where

import           Crypto.Hash.SHA256     as SHA256
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base16 as BS16

type SigningSecret = ByteString
type Timestamp = ByteString
type Signature = ByteString
type Body = ByteString

verify :: SigningSecret -> Timestamp -> Body -> Signature -> Bool
verify secret ts body sig = build content == sig
  where
    build :: ByteString -> ByteString
    build = ("v0=" <>) . BS16.encode . SHA256.hmac secret

    content :: ByteString
    content = mconcat ["v0", ":",  ts, ":", body]
