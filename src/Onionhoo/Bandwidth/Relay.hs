{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A Bandwidth document for relays
module Onionhoo.Bandwidth.Relay where

import Onionhoo.History.Graph
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types

-- | Contains Bandwidth information for a relay
data Relay =
  Relay {fingerprint :: String
        ,writeHistory :: Maybe Object
        ,readHistory :: Maybe Object}
  deriving (Show)

$(deriveFromJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}
                 ''Relay)
