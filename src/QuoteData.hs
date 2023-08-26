{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module QuoteData (QuoteData) where

import Data.ByteString.Char8 (unpack)
import Data.Csv (Field, FromField (..), FromNamedRecord, Parser)
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import GHC.Generics (Generic)

data QuoteData = QuoteData
    { day :: Day
    , volume :: Int
    , open :: Double
    , close :: Double
    , high :: Double
    , low :: Double
    }
    deriving (Generic, FromNamedRecord)

instance FromField Day where
    parseField :: Field -> Parser Day
    parseField field = parseTimeM True defaultTimeLocale "%Y-%m-%d" $ unpack field