{-# LANGUAGE OverloadedStrings #-}

module Bot.Handler.EventSubscriber
  ( Response
  , Handler
  , runHandler
  , defaultHandler
  , response
  ) where

import           AWSLambda.Events.APIGateway (response)
import qualified AWSLambda.Events.APIGateway as API
import qualified Bot.AWS.SecretsManager      as SM
import qualified Bot.Slack.Auth              as SlackAuth
import           Bot.Slack.Event             (Event (URLVerification))
import qualified Bot.Slack.Event             as Ev
import           Control.Exception.Safe      (Exception, MonadThrow, catch,
                                              throw)
import           Control.Lens                ((.~), (?~), (^.))
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import qualified Data.Aeson                  as Aeson
import           Data.ByteString             (ByteString)
import qualified Data.Map                    as M
import           Data.Maybe                  (fromMaybe)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as TE
import           System.Environment          (getEnv)

type Request = API.APIGatewayProxyRequest ByteString
type Response = API.APIGatewayProxyResponse ByteString
type Handler = Event -> IO Response

newtype HandlerException = HandlerException Response deriving (Show)
instance Exception HandlerException

runHandler :: Handler -> IO ()
runHandler handler = API.apiGatewayMain $ \req -> do
  verify req
  event <- decode req
  handler event
  `catch` \(HandlerException res) -> return res

defaultHandler :: Handler
defaultHandler (URLVerification ev) =
  let challenge = Ev._urlVerificationEventChallenge ev in
    return $ (API.responseBody ?~ TE.encodeUtf8 challenge) API.responseOK
defaultHandler _ = return API.responseBadRequest

verify :: (MonadThrow m, MonadIO m) => Request -> m ()
verify req = do
  secrets <- liftIO $ SM.getSecrets "for-bot"
  let secret = M.lookup "SLACK_SIGNING_SECRET" secrets
  let headers = req ^. API.agprqHeaders
  if fromMaybe False
    ( SlackAuth.verify
        <$> (TE.encodeUtf8 <$> secret)
        <*> lookup "X-Slack-Request-Timestamp" headers
        <*> req ^. API.requestBody
        <*> lookup "X-Slack-Signature" headers
    )
    then return ()
    else throw $ HandlerException responseUnauthorized

  where
    responseUnauthorized :: Response
    responseUnauthorized = API.response 401

decode :: (MonadThrow m) => Request -> m Event
decode req = case req ^. API.requestBody >>= Aeson.decodeStrict of
  Just body -> return body
  Nothing   -> throw $ HandlerException API.responseBadRequest
