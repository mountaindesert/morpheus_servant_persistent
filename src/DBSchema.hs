{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module DBSchema (Patient(..), Symptom(..), migrateAll) where

import           Data.Aeson           (FromJSON (parseJSON), Object,
                                       ToJSON (toJSON), object, withObject,
                                       (.:), (.=))
import           Data.Aeson.Types     (Pair, Parser)
import           Data.Text            (Text)
import           Data.Time            (UTCTime)
import           Database.Persist     (Entity (..))
import           Database.Persist.Sql (fromSqlKey, toSqlKey)
import           Database.Persist.TH  (mkMigrate, mkPersist, persistLowerCase,
                                       share, sqlSettings)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
  Patient sql=test_patients
    name Text
    email Text
    age Int
    favoriteColor (Maybe Text)
    birthdate UTCTime default=CURRENT_TIMESTAMP
    UniqueEmail email
    deriving Show Read Eq

  Symptom sql=test_symptoms
    description Text
    severity Int
    reportedAt UTCTime default=CURRENT_TIMESTAMP
    patientId PatientId
    deriving Show Read Eq
|]

instance ToJSON (Entity Patient) where
    toJSON (Entity pid patient) =
        object $
            "id" .= fromSqlKey pid : patientPairs patient

instance ToJSON Patient where
    toJSON patient = object $ patientPairs patient

patientPairs :: Patient -> [Pair]
patientPairs Patient{..} =
    [ "name" .= patientName
    , "email" .= patientEmail
    , "age" .= patientAge
    , "favoriteColor" .= patientFavoriteColor
    ]

instance FromJSON (Entity Patient) where
    parseJSON = withObject "Patient Entity" $ \o -> do
        patient <- parsePatient o
        pid <- o .: "id"
        return $ Entity (toSqlKey pid) patient

instance FromJSON Patient where
    parseJSON = withObject "Patient" parsePatient

parsePatient :: Object -> Parser Patient
parsePatient o = do
    patientName <- o .: "name"
    patientEmail <- o .: "email"
    patientAge <- o .: "age"
    patientBirthdate <- o .: "birthDate"
    patientFavoriteColor <- o .: "favoriteColor"
    return Patient{..}

instance ToJSON (Entity Symptom) where
    toJSON (Entity sid symptom) =
        object $
            "id" .= fromSqlKey sid : symptomPairs symptom
instance ToJSON Symptom where
    toJSON symptom = object $ symptomPairs symptom

symptomPairs :: Symptom -> [Pair]
symptomPairs Symptom{..} =
    [ "description" .= symptomDescription
    , "severity" .= symptomSeverity
    , "reportedAt" .= symptomReportedAt
    , "patientId" .= fromSqlKey symptomPatientId
    ]

instance FromJSON (Entity Symptom) where
    parseJSON = withObject "Symptom Entity" $ \o -> do
        symptom <- parseSymptom o
        sid <- o .: "id"
        return $ Entity (toSqlKey sid) symptom

instance FromJSON Symptom where
    parseJSON = withObject "Symptom" parseSymptom

parseSymptom :: Object -> Parser Symptom
parseSymptom o = do
    symptomDescription <- o .: "description"
    symptomSeverity <- o .: "severity"
    symptomReportedAt <- o .: "reportedAt"
    symptomPatientId <- o .: "patientId"
    return Symptom{..}
