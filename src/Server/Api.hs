{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Server.Api
  (API, server)
where

import           Config                     (App, AppConfig (..), AppState (..),
                                             appConfig, runIO)
import           Control.Monad.Reader       (asks, liftIO)
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.List.NonEmpty         (NonEmpty ((:|)))
import           Data.Morpheus              (httpPlayground)
import           Data.Morpheus.Types        (GQLRequest, GQLResponse)
import           Data.Text                  (Text, pack)
import           Data.Time.Clock            (getCurrentTime)
import           Data.Typeable              (Typeable)
import           GQLSchema                  (gqlApi)
import           Network.HTTP.Media         ((//), (/:))
import           Servant                    (Accept (..), Get, Handler, JSON,
                                             MimeRender (..), PlainText, Post,
                                             ReqBody, Server, (:<|>) (..), (:>))

data HTML deriving (Typeable)

instance Accept HTML where
  contentTypes _ = "text" // "html" /: ("charset", "utf-8") :| ["text" // "html"]

instance MimeRender HTML ByteString where
  mimeRender _ = id

type API = "status" :> Get '[PlainText] Text
        :<|> "graphql" :> ReqBody '[JSON] GQLRequest :> Post '[JSON] GQLResponse
        :<|> "schema" :> Get '[HTML] ByteString
server :: AppState -> Server API
server ctx = statusHandler ctx :<|> gqlHandler ctx :<|> pure httpPlayground

statusHandler :: AppState -> Handler Text
statusHandler s = runServant s $ do
  now <- liftIO getCurrentTime
  cfg <- asks appConfig
  pure $ "API running on port " <> pack (show $ servicePort cfg) <> " at " <> pack (show now)

gqlHandler :: AppState -> GQLRequest -> Handler GQLResponse
gqlHandler s bs = runServant s (gqlApi bs)

runServant :: AppState -> App a -> Handler a
runServant s app = liftIO $ runIO s app
