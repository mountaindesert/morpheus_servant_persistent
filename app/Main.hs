{-# LANGUAGE LambdaCase #-}

module Main (main) where

import           Config             (appVersion, loadConfig)
import qualified Data.Text.IO       as Text (putStrLn)
import           Database           (migrate, showMigrations)
import           Options            (DbOptions (..), Options (..), getOptions)
import           Server.Servant     (startApi)
import           System.IO          (BufferMode (LineBuffering), hSetBuffering,
                                     stderr, stdout)
import           UnliftIO.Exception (tryAny)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  getOptions >>= \case
    Version -> Text.putStrLn appVersion
    API -> startApi
    Config -> loadConfig >>= print
    DB Migrate -> tryAny migrate >>= \case
        Left err -> putStrLn $ "Migration failed: " <> show err
        Right _ -> putStrLn "Migration complete"
    DB ShowMigrations -> do
      migrations <- showMigrations
      mapM_ Text.putStrLn migrations
    DB Seed -> putStrLn "Seed"

