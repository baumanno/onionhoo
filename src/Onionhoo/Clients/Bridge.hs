{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A Clients document for bridges
module Onionhoo.Clients.Bridge where

import Onionhoo.History.Graph
import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Text (Text)

-- | Contains Clients information for a bridge
data Bridge =
  Bridge {fingerprint :: Text
         ,averageClients :: Maybe ClientHistory}
  deriving (Show)

data ClientHistory =
  ClientHistory {oneWeek :: Maybe Graph
                ,oneMonth :: Maybe Graph
                ,threeMonths :: Maybe Graph
                ,oneYear :: Maybe Graph
                ,fiveYears :: Maybe Graph}
  deriving (Show)

$(deriveFromJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}
                 ''Bridge)

instance FromJSON ClientHistory where
  parseJSON (Object v) = 
    ClientHistory <$>
    v .:? "1_week" <*>
    v .:? "1_month" <*>
    v .:? "3_months" <*>
    v .:? "1_year" <*>
    v .:? "5_years"
  parseJSON _ = mzero
