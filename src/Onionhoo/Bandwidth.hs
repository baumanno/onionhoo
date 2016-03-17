{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A Bandwidth-document for bridges and relays
module Onionhoo.Bandwidth where

import qualified Onionhoo.Bandwidth.Bridge as B (Bridge)
import qualified Onionhoo.Bandwidth.Relay as R (Relay)
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Text (Text)

-- | A Bandwidth document containing information on bridges and relays
data Bandwidth =
  Bandwidth {version :: Text -- ^ the current API version
            ,nextMajorVersionScheduled :: Maybe Text -- ^ optional
            ,relaysPublished :: Text -- ^ UTC date when the list of relays was published
            ,relays :: [R.Relay] -- ^ list of relays
            ,bridgesPublished :: Text -- ^ UTC date when the list of bridges was published
            ,bridges :: [B.Bridge]} -- ^ list of bridges
  deriving (Show)

$(deriveFromJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}
                 ''Bandwidth)
