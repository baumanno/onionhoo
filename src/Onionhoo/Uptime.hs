{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A Uptime-document for bridges and relays
module Onionhoo.Uptime where

import qualified Onionhoo.Uptime.Bridge as B (Bridge)
import qualified Onionhoo.Uptime.Relay as R (Relay)

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types

-- | A Uptime document containing information on bridges and relays
data Uptime = Uptime
  { version :: String -- ^ the current API version
  , nextMajorVersionScheduled :: Maybe String -- ^ optional
  , relaysPublished :: String -- ^ UTC date when the list of relays was published
  , relays :: [R.Relay] -- ^ list of relays
  , bridgesPublished :: String -- ^ UTC date when the list of bridges was published
  , bridges :: [B.Bridge] -- ^ list of bridges
  } deriving (Show)

$(deriveFromJSON defaultOptions {fieldLabelModifier = camelTo2 '_'} ''Uptime)
