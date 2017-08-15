{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A detail document for bridges
module Onionhoo.Detail.Bridge where

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types

-- | Contains detail information for a bridge
data Bridge = Bridge
  { nickname :: Maybe String
  , hashedFingerprint :: String
  , orAddresses :: [String]
  , lastSeen :: String
  , firstSeen :: String
  , running :: Bool
  , flags :: Maybe [String]
  , lastRestarted :: Maybe String
  , advertisedBandwidth :: Maybe Int
  , platform :: Maybe String
  , transports :: Maybe [String]
  } deriving (Show)

$(deriveFromJSON defaultOptions {fieldLabelModifier = camelTo2 '_'} ''Bridge)
