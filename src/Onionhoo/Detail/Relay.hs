{-# LANGUAGE OverloadedStrings, DeriveGeneric, TemplateHaskell #-}

-- | A detail document for relays
module Onionhoo.Detail.Relay where

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import GHC.Generics

-- | Contains detail information for a relay
data Relay = Relay
  { nickname :: Maybe String
  , fingerprint :: String
  , orAddresses :: [String]
  , exitAddresses :: Maybe [String]
  , dirAddress :: Maybe String
  , lastSeen :: String
  , lastChangedAddressOrPort :: String
  , firstSeen :: String
  , running :: Bool
  , hibernating :: Maybe Bool
  , flags :: Maybe [String]
  , country :: Maybe String
  , countryName :: Maybe String
  , regionName :: Maybe String
  , cityName :: Maybe String
  , latitude :: Maybe Double
  , longitude :: Maybe Double
  , asNumber :: Maybe String
  , asName :: Maybe String
  , consensusWeight :: Int
  , hostName :: Maybe String
  , lastRestarted :: Maybe String
  , bandwidthRate :: Maybe Int
  , bandwidthBurst :: Maybe Int
  , observedBandwidth :: Maybe Int
  , advertisedBandwidth :: Maybe Int
  , exitPolicy :: Maybe [String]
  , exitPolicySummary :: Maybe Object
  , exitPolicyV6Summary :: Maybe Object
  , contact :: Maybe String
  , platform :: Maybe String
  , recommendedVersion :: Maybe Bool
  , effectiveFamily :: Maybe [String]
  , allegedFamily :: Maybe [String]
  , indirectFamily :: Maybe [String]
  , consensusWeightFraction :: Maybe Double
  , guardProbability :: Maybe Double
  , middleProbability :: Maybe Double
  , exitProbability :: Maybe Double
  , measured :: Maybe Bool
  } deriving (Show, Generic)

--instance FromJSON Relay
$(deriveFromJSON defaultOptions {fieldLabelModifier = camelTo2 '_'} ''Relay)
