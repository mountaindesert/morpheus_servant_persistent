{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}


module Database (getEntity', getPatientsByName, getPatientsByFavoriteColor, getPatientsBySymptom, insertPatient, insertSymptom, migrate, showMigrations, fromSqlKey, toSqlKey, Key, Entity(..)) where

import           Config                          (App, AppState (..), loadState)
import           Control.Monad.Logger            (logInfoNS, runStdoutLoggingT)
import           Control.Monad.Reader            (asks, liftIO)
import           Data.Text                       (Text)
import           Database.Esqueleto.Experimental (Entity (..),
                                                  PersistStoreWrite (insert),
                                                  desc, from, innerJoin, just,
                                                  on, orderBy, select, table,
                                                  val, where_, (:&) (..), (==.),
                                                  (^.))
import           Database.Persist.Postgresql     (Key, PersistEntity,
                                                  PersistEntityBackend,
                                                  SqlPersistT, fromSqlKey, get,
                                                  runMigration, runSqlPool,
                                                  showMigration, toSqlKey)
import           Database.Persist.SqlBackend     (SqlBackend)
import           DBSchema                        (Patient (..), Symptom (..),
                                                  migrateAll)

migrate :: IO ()
migrate = runStdoutLoggingT $ do
    logInfoNS "DB" "Running migrations"
    s <- loadState
    liftIO $ runSqlPool (runMigration migrateAll) (dbPool s)

showMigrations :: IO [Text]
showMigrations = runStdoutLoggingT $ do
    logInfoNS "DB" "Getting migrations"
    s <- loadState
    liftIO $ runSqlPool (showMigration migrateAll) (dbPool s)

runDb ::SqlPersistT IO a -> App a
runDb action = do
  pool <- asks dbPool
  liftIO $ runSqlPool action pool

getEntity' :: (PersistEntityBackend record ~ SqlBackend, PersistEntity record) => Key record -> App (Maybe record)
getEntity' uid = runDb $ get uid

insertPatient :: Patient -> App (Key Patient)
insertPatient patient = runDb $ insert patient

insertSymptom :: Symptom -> App (Key Symptom)
insertSymptom symptom = runDb $ insert symptom

getPatientsByName :: Text -> App [Entity Patient]
getPatientsByName name = runDb $ select $ do
  p <- from $ table @Patient
  where_ (p ^. #name ==. val name)
  orderBy [desc (p ^. #age)]
  pure p

getPatientsByFavoriteColor :: Text -> App [Entity Patient]
getPatientsByFavoriteColor color = runDb $ select $ do
  p <- from $ table @Patient
  where_ (p ^. #favoriteColor ==. just (val color))
  orderBy [desc (p ^. #name)]
  pure p

getPatientsBySymptom :: Text -> App [Entity Patient]
getPatientsBySymptom symptom = runDb $ select $ do
    (p :& s) <-
        from $ table @Patient
        `innerJoin` table @Symptom
        `on` (\(p :& s) ->
            p ^. #id ==. s ^. #patientId)
    where_ (s ^. #description ==. val symptom)
    orderBy [desc (p ^. #name)]
    pure p
