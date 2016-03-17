{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A Clients-document for bridges and relays
module Onionhoo.Clients where

import qualified Onionhoo.Clients.Bridge as B (Bridge)

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Text (Text)

-- | A Clients document containing information on bridges and relays
data Clients =
  Clients {version :: Text -- ^ the current API version
          ,nextMajorVersionScheduled :: Maybe Text -- ^ optional
          ,relaysPublished :: Text -- ^ UTC date when the list of relays was published
          ,relays :: Array -- ^ list of relays
          ,bridgesPublished :: Text -- ^ UTC date when the list of bridges was published
          ,bridges :: [B.Bridge]} -- ^ list of bridges
  deriving (Show)

$(deriveFromJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}
                 ''Clients)
