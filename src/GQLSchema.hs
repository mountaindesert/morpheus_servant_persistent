{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-} --TODO: use newtype wrappers to avoid orphan instances

module GQLSchema (gqlApi) where

import           Config                    (App)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Data.FileEmbed            (makeRelativeToProject)
import           Data.Int                  (Int64)
import           Data.Morpheus             (interpreter)
import           Data.Morpheus.Document    (importGQLDocument)
import           Data.Morpheus.Types       (Arg (Arg), DecodeScalar (..),
                                            EncodeScalar (..), GQLRequest,
                                            GQLResponse, ID (..), ResolverM,
                                            RootResolver (..),
                                            ScalarValue (String), Undefined)
import           Data.String.Conversions   (cs)
import           Data.Text                 (Text)
import           Data.Time                 (UTCTime)
import           Data.Time.Clock           (getCurrentTime)
import           Data.Time.Format.ISO8601  (iso8601ParseM, iso8601Show)
import qualified Database                  as DB
import qualified DBSchema                  as DBS
import           Text.Read                 (readMaybe)

makeRelativeToProject "src/schema.gql" >>= importGQLDocument

instance DecodeScalar UTCTime where
    decodeScalar (String utcTime) = iso8601ParseM $ cs utcTime
    decodeScalar _                = Left "Invalid Value for DateTime"

instance EncodeScalar UTCTime where
    encodeScalar utcTime = String $ cs $ iso8601Show utcTime

rootResolver :: RootResolver App () Query Mutation Undefined
rootResolver = RootResolver
    { queryResolver = Query { patient, patientsByFavoriteColor, patientsBySymptom, patientsByName }
    , mutationResolver = Mutation { createPatient = resolveCreatePatient, reportSymptom = resolveReportSymptom }
    , subscriptionResolver = undefined
    }
    where
        --resolvers--
        patient (Arg pid) = do

            case toPatientId pid of
                Nothing -> pure Nothing
                Just patientId -> do
                    mPatient <- lift $ DB.getEntity' patientId
                    pure $ toGQLPatient patientId <$> mPatient

        patientsByFavoriteColor (Arg color) = do
            pts <- lift $ DB.getPatientsByFavoriteColor color
            pure $ map patientEntitytoGQLPatient pts

        patientsBySymptom (Arg symptom) = do
            pts <- lift $ DB.getPatientsBySymptom symptom
            pure $ map patientEntitytoGQLPatient pts

        patientsByName (Arg name) = do
            pts <- lift $ DB.getPatientsByName name
            pure $ map patientEntitytoGQLPatient pts

        --helpers--

        patientEntitytoGQLPatient dbe = toGQLPatient (DB.entityKey dbe) (DB.entityVal dbe)
        toGQLPatient patientId pt = Patient { id = pure $ ID $ cs $ show $ DB.fromSqlKey patientId
                                , name = pure $ DBS.patientName pt
                                , email = pure $ DBS.patientEmail pt
                                , age = pure $ DBS.patientAge pt
                                , birthdate = pure $ DBS.patientBirthdate pt
                                , favoriteColor = pure $ DBS.patientFavoriteColor pt
                                }


resolveCreatePatient :: CreatePatientArgs -> ResolverM () App Patient
resolveCreatePatient CreatePatientArgs { name, email, age, favoriteColor, birthdate } = do

    pid <- lift $ DB.insertPatient DBS.Patient { DBS.patientName = name
                                        , DBS.patientEmail = email
                                        , DBS.patientAge = age
                                        , DBS.patientBirthdate = birthdate
                                        , DBS.patientFavoriteColor = favoriteColor
                                        }
    pure Patient
        { id = pure $ ID $ cs $ show $ DB.fromSqlKey pid
        , name = pure name
        , email = pure email
        , age = pure age
        , birthdate = pure birthdate
        , favoriteColor = pure favoriteColor
        }

resolveReportSymptom :: (Monad m) => ReportSymptomArgs -> ResolverM () App (Maybe (Symptom m))
resolveReportSymptom ReportSymptomArgs { description, severity, patientId } = do
    now <- liftIO getCurrentTime
    let msymp = toDBSymptom description severity now <$> toPatientId patientId

    case msymp of
        Nothing -> pure Nothing
        Just symp -> do
            sid <- lift $ DB.insertSymptom symp
            pure $ Just Symptom { id = pure $ ID $ cs $ show $ DB.fromSqlKey sid
                        , description = pure description
                        , reportedAt = pure now
                        , severity = pure severity
                        , patientId = pure patientId
                    }
    where
        toDBSymptom d s now pid = DBS.Symptom
            { DBS.symptomDescription = d
            , DBS.symptomSeverity =  s
            , DBS.symptomReportedAt = now
            , DBS.symptomPatientId = pid
            }

gqlApi :: GQLRequest -> App GQLResponse
gqlApi = interpreter rootResolver

toPatientId :: ID -> Maybe(DB.Key DBS.Patient)
toPatientId (ID pid) =
    let mInt64:: Maybe Int64 = readMaybe $ cs pid
    in DB.toSqlKey <$> mInt64
