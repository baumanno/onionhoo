{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A detail document for bridges
module Onionhoo.Detail.Bridge (Bridge) where

import Data.Text (Text)
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import GHC.Generics

-- | Contains detail information for a bridge
data Bridge =
  Bridge {nickname :: Maybe Text
         ,hashedFingerprint :: Text
         ,orAddresses :: [Text]
         ,lastSeen :: Text
         ,firstSeen :: Text
         ,running :: Bool
         ,flags :: Maybe [Text]
         ,lastRestarted :: Maybe Text
         ,advertisedBandwidth :: Maybe Int
         ,platform :: Maybe Text
         ,transports :: Maybe [Text]}
  deriving (Show,Generic)

$(deriveFromJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}
                 ''Bridge)
