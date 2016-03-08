{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A Weights document for relays
module Onionhoo.Weights.Relay (Relay) where

import Data.Text (Text)
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types

-- | Contains Weights information for a relay
data Relay =
  Relay {fingerprint :: Text
        ,consensusWeightFraction :: Maybe Object
        ,guardProbability :: Maybe Object
        ,middleProbability :: Maybe Object
        ,exitProbability :: Maybe Object
        ,consensusWeight :: Maybe Object
        }
  deriving (Show)

$(deriveFromJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}
                 ''Relay)
