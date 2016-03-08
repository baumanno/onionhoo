{-# LANGUAGE OverloadedStrings, DeriveGeneric, TemplateHaskell #-}

-- | A detail document for relays
module Onionhoo.Detail.Relay (Relay) where

import Data.Text (Text)
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.TH
import GHC.Generics

-- | Contains detail information for a relay
data Relay =
  Relay {nickname :: Maybe Text
        ,fingerprint :: Text
        ,orAddresses :: [Text]
        ,exitAddresses :: Maybe [Text]
        ,dirAddress :: Text
        ,lastSeen :: Text
        ,lastChangedAddressOrPort :: Text
        ,firstSeen :: Text
        ,running :: Bool
        ,hibernating :: Maybe Bool
        ,flags :: Maybe [Text]
        ,country :: Maybe Text
        ,countryName :: Maybe Text
        ,regionName :: Maybe Text
        ,cityName :: Maybe Text
        ,latitude :: Maybe Double
        ,longitude :: Maybe Double
        ,asNumber :: Maybe Text
        ,asName :: Maybe Text
        ,consensusWeight :: Int
        ,hostName :: Maybe Text
        ,lastRestarted :: Maybe Text
        ,bandwidthRate :: Maybe Int
        ,bandwidthBurst :: Maybe Int
        ,observedBandwidth :: Maybe Int
        ,advertisedBandwidth :: Maybe Int
        ,exitPolicy :: Maybe [Text]
        ,exitPolicySummary :: Maybe Object
        ,exitPolicyV6Summary :: Maybe Object
        ,contact :: Maybe Text
        ,platform :: Maybe Text
        ,recommendedVersion :: Maybe Bool
        ,effectiveFamily :: Maybe [Text]
        ,allegedFamily :: Maybe [Text]
        ,indirectFamily :: Maybe [Text]
        ,consensusWeightFraction :: Maybe Double
        ,guardProbability :: Maybe Double
        ,middleProbability :: Maybe Double
        ,exitProbability :: Maybe Double
        ,measured :: Maybe Bool}
  deriving (Show,Generic)

--instance FromJSON Relay
$(deriveFromJSON defaultOptions { fieldLabelModifier = camelTo2 '_' } ''Relay)
