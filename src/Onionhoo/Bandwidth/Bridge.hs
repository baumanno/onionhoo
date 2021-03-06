{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A Bandwidth document for bridges
module Onionhoo.Bandwidth.Bridge (Bridge) where

import Data.Text (Text)
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types

-- | Contains Bandwidth information for a bridge
data Bridge =
  Bridge {fingerprint :: Text
         ,writeHistory :: Maybe Object
         ,readHistory :: Maybe Object}
  deriving (Show)

$(deriveFromJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}
                 ''Bridge)
