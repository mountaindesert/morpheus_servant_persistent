{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Servant (startApi) where

import           Config                                    (AppConfig (..),
                                                            AppState (..),
                                                            appConfig,
                                                            appVersion,
                                                            loadState, runIO)
import           Control.Monad.Logger                      (logInfoNS,
                                                            runStdoutLoggingT,
                                                            toLogStr)
import           Control.Monad.Reader                      (liftIO)
import           Data.Aeson                                (decodeStrict)
import qualified Data.ByteString.Char8                     as B
import           Data.Default                              (def)
import           Data.Morpheus.Types                       (GQLRequest (..))
import           Data.String.Conversions                   (cs)
import           Data.Text                                 (Text, pack)
import           Data.Time.Clock                           (getCurrentTime)
import           Data.Time.Format                          (FormatTime,
                                                            defaultTimeLocale,
                                                            formatTime)
import           Network.Wai                               (Middleware)
import qualified Network.Wai.Handler.Warp                  as Warp
import           Network.Wai.Middleware.AddHeaders         (addHeaders)
import           Network.Wai.Middleware.Cors               (CorsResourcePolicy (..),
                                                            cors,
                                                            simpleResponseHeaders)
import           Network.Wai.Middleware.RequestLogger      (OutputFormat (..),
                                                            OutputFormatterWithDetails,
                                                            RequestLoggerSettings (..),
                                                            mkRequestLogger)
import           Network.Wai.Middleware.RequestLogger.JSON (formatAsJSON)
import qualified Servant
import           Server.Api                                (API, server)
import           System.IO.Unsafe                          (unsafePerformIO)
import           System.Posix.Process                      (getProcessID)

app :: AppState -> Servant.Application
app cfg = unsafePerformIO customLogger $ heads $ addCors $ Servant.serve (Servant.Proxy :: Servant.Proxy API) (server cfg)
    where
        heads = addHeaders [("API-Version", cs appVersion)]
        addCors = cors (const $ Just corsResourcePolicy)
startApi :: IO ()
startApi = do
  s <- runStdoutLoggingT $ do
    now <- liftIO getCurrentTime
    processId <- liftIO getProcessID
    s <- loadState
    logInfoNS "INIT" $ "API version " <> appVersion <> " booting at " <> formatDate now <> " with PID " <> pack (show processId)
    return s
  runIO s $ do
    let p = servicePort $ appConfig s
    logInfoNS "INIT" $ "API listening on " <> pack (show p)
    liftIO $ Warp.run p (app s)

formatDate :: (FormatTime t) => t -> Text
formatDate =
  pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

corsResourcePolicy :: CorsResourcePolicy
corsResourcePolicy = CorsResourcePolicy
    { corsOrigins = Nothing
    , corsMethods = ["GET", "HEAD", "OPTIONS", "PUT", "POST", "DELETE"]
    , corsRequestHeaders = simpleResponseHeaders <> ["Authorization"]
    , corsExposedHeaders = Nothing
    , corsMaxAge = Nothing
    , corsVaryOrigin = False
    , corsRequireOrigin = False
    , corsIgnoreFailures = False
    }

-- customLogger2 :: IO Middleware
-- customLogger2 = mkRequestLogger def {outputFormat = DetailedSettings dontLogNoisyQueries }
--   where
--     dontLogNoisyQueries :: DetailedSettings
--     dontLogNoisyQueries useColors mModifyParams mFilterRequests mPrelogRequests = _



      -- let
      --   mFieldName = operationName =<< decodeStrict (foldr (<>) B.empty reqBody) --join $ operationName <$> decodeStrict (foldl (<>) B.empty reqBody)
      -- in
      --   case mFieldName of
      --     Nothing -> logStdoutDev useColors mModifyParams mFilterRequests mPrelogRequests
      --     Just fn ->
      --       if fn == "IntrospectionQuery"
      --         then toLogStr B.empty
      --         else logStdoutDev useColors mModifyParams mFilterRequests mPrelogRequests

customLogger :: IO Middleware
customLogger = mkRequestLogger def {outputFormat = CustomOutputFormatWithDetails dontLogIntrospectionQueries }
  where
    dontLogIntrospectionQueries :: OutputFormatterWithDetails
    dontLogIntrospectionQueries date req status responseSize duration reqBody response =
      let
        mFieldName = operationName =<< decodeStrict (foldr (<>) B.empty reqBody) --join $ operationName <$> decodeStrict (foldl (<>) B.empty reqBody)
      in
        case mFieldName of
          Nothing -> formatAsJSON date req status responseSize duration reqBody response
          Just fn ->
            if fn == "IntrospectionQuery"
              then toLogStr B.empty
              else formatAsJSON date req status responseSize duration reqBody response

