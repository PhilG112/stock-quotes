{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module QuoteData where

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

data QField = Open | Close | High | Low | Volume
    deriving (Eq, Ord, Show, Enum, Bounded)

field2Fun :: QField -> QuoteData -> Double
field2Fun Open qd = open qd
field2Fun Close qd = close qd
field2Fun High qd = high qd
field2Fun Low qd = low qd
field2Fun Volume qd = fromIntegral (volume qd)


