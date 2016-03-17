{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A detail-document for bridges and relays
module Onionhoo.Detail where

import qualified Onionhoo.Detail.Bridge as B (Bridge)
import qualified Onionhoo.Detail.Relay as R (Relay)
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types

-- | A summary document containing information on bridges and relays
data Detail =
  Detail {version :: String -- ^ the current API version
         ,nextMajorVersionScheduled :: Maybe String -- ^ optional
         ,relaysPublished :: String -- ^ UTC date when the list of relays was published
         ,relays :: [R.Relay] -- ^ list of relays
         ,bridgesPublished :: String -- ^ UTC date when the list of bridges was published
         ,bridges :: [B.Bridge]} -- ^ list of bridges
  deriving (Show)

$(deriveFromJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}
                 ''Detail)
