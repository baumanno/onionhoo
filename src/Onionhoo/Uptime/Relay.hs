{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A Uptime document for relays
module Onionhoo.Uptime.Relay (Relay) where

import Onionhoo.History.Graph
import Control.Monad (mzero)
import Data.Text (Text)
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types

-- | Contains Uptime information for a relay
data Relay =
  Relay {fingerprint :: Text
        ,uptime :: Maybe UptimeHistory
        ,flags :: Maybe Object}
  deriving (Show)

data UptimeHistory =
  UptimeHistory {oneWeek :: Maybe Graph
                ,oneMonth :: Maybe Graph
                ,threeMonths :: Maybe Graph
                ,oneYear :: Maybe Graph
                ,fiveYears :: Maybe Graph}
  deriving (Show)

$(deriveFromJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}
                 ''Relay)

instance FromJSON UptimeHistory where
  parseJSON (Object v) = 
    UptimeHistory <$>
    v .:? "1_week" <*>
    v .:? "1_month" <*>
    v .:? "3_months" <*>
    v .:? "1_year" <*>
    v .:? "5_years"
  parseJSON _ = mzero
