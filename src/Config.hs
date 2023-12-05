{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}



module Config (App(..), AppState(..), AppConfig(..), appVersion, loadConfig, loadState, runIO) where

import           Control.Monad.IO.Unlift     (MonadIO, MonadUnliftIO, liftIO)
import           Control.Monad.Logger        (LoggingT, MonadLogger,
                                              MonadLoggerIO, runStdoutLoggingT)
import           Control.Monad.Reader        (MonadReader, ReaderT, runReaderT)
import           Data.Text                   (Text)
import           Database.Persist.Postgresql (ConnectionPool, ConnectionString,
                                              PostgresConf (..),
                                              PostgresConfHooks,
                                              createPostgresqlPoolWithConf,
                                              defaultPostgresConfHooks)
import           GHC.Generics                (Generic)
import           System.Envy                 (DefConfig (..), FromEnv,
                                              decodeEnv)
import           UnliftIO.Exception          (Exception, Typeable)

newtype App a = App
    { runApp :: ReaderT AppState (LoggingT IO) a
    } deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppState, MonadLogger)

data AppState = AppState
    { appConfig :: AppConfig
    , dbPool    :: ConnectionPool
    }

data AppConfig = AppConfig
    { servicePort :: Int
    , postgres    :: ConnectionString
    , environment :: Text
    } deriving (Generic, Show)
instance FromEnv AppConfig
instance DefConfig AppConfig where
    defConfig = AppConfig
        { servicePort = 5051
        , postgres = "host=localhost port=5432 user=postgres dbname=postgres password=postgres"
        , environment = "development"
        }

data AppException = EnvException String
                  | DbException String
    deriving (Show, Typeable)
instance Exception AppException

loadConfig :: (MonadIO m) => m AppConfig
loadConfig = do
    cfg <- liftIO decodeEnv
    case cfg of
        Left err -> do
            liftIO $ putStrLn $ "Error loading config from environment variables: " <> show err <> "\nUsing default config"
            pure defConfig
        Right c  -> pure c

data PGConfig = PGConfig {
    pgConf      :: PostgresConf,
    pgConfHooks :: PostgresConfHooks
}

dbConnectInfo :: AppConfig -> PGConfig
dbConnectInfo cfg =
    PGConfig { pgConf = PostgresConf (postgres cfg) poolStripes poolIdleTimeout poolSize -- this can be tailored based on environment
                    , pgConfHooks = defaultPostgresConfHooks
                    }
      where
    poolStripes = 1 -- how many stripes to divide the pool into
    poolIdleTimeout = 5 -- time to keep unused connection open in seconds
    poolSize = 8 -- number of connections that should be held in the connection pool

connectDb :: (MonadLoggerIO m, MonadUnliftIO m) => PGConfig -> m ConnectionPool
connectDb pgConfig =
    createPostgresqlPoolWithConf (pgConf pgConfig)  (pgConfHooks pgConfig)

mkState :: (MonadLoggerIO m, MonadUnliftIO m) => (AppConfig -> PGConfig) -> m AppState
mkState toDbInfo = do
    e <- loadConfig
    db <- connectDb $ toDbInfo e
    pure
        AppState
            { appConfig = e
            , dbPool = db
            }

loadState :: (MonadLoggerIO m, MonadUnliftIO m) => m AppState
loadState = mkState dbConnectInfo

appVersion :: Text
appVersion = "0.0.1" -- Eventually can hook this up to github to display the hash of the commit that is currently running in whichever environment

runIO :: AppState -> App a -> IO a
runIO s app = runStdoutLoggingT (runReaderT (runApp app) s)
