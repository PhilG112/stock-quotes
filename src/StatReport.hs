{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module StatReport where

import Colonnade (Colonnade, Headed, ascii, headed)
import Data.Foldable (maximumBy, minimumBy)
import Data.Ord (comparing)
import Data.Time (diffDays)
import Fmt
import QuoteData (QField (Volume), QuoteData (day), field2Fun)

decimalPlacesFloating :: Integer
decimalPlacesFloating = 2

data StatValue = StatValue
    { decimalPlaces :: Int
    , value :: Double
    }

instance Buildable StatValue where
    build :: StatValue -> Builder
    build sv = fixedF (decimalPlaces sv) (value sv)

data StatEntry = StatEntry
    { qField :: QField
    , meanVal :: StatValue
    , minVal :: StatValue
    , maxVal :: StatValue
    , daysBetweenMinMax :: Int
    }

{- FOURMOLU_DISABLE -}
instance Buildable StatEntry where
    build :: StatEntry -> Builder
    build StatEntry {..} =
        "Stats for"+||qField||+ ": "
            +|meanVal|+ " (mean), "
            +|minVal|+ " (min), "
            +|maxVal|+ " (max) "
            +|daysBetweenMinMax|+ " (days)"
{- FOURMOLU_DISABLE -}

textReport :: [StatEntry] -> String
textReport entries = ascii colStats entries
    where
        colStats :: Colonnade Headed StatEntry String
        colStats = mconcat
            [ headed "Quote Field" (show . qField)
            , headed "Mean" (pretty . meanVal)
            , headed "Min" (pretty . minVal)
            , headed "Max" (pretty . maxVal)
            , headed "Days between Min/Max" (pretty . daysBetweenMinMax)
            ]

showPrice :: Double -> Builder
showPrice = fixedF (fromIntegral decimalPlacesFloating)

mean :: (Fractional a, Foldable t) => t a -> a
mean xs = sum xs / fromIntegral (length xs)

computeMinMaxDays ::
    (Ord a, Foldable t) =>
    (QuoteData -> a) ->
    t QuoteData ->
    (a, a, Int)
computeMinMaxDays prop quotes = (prop minQuote, prop maxQuote, days)
    where
        cmp :: QuoteData -> QuoteData -> Ordering
        cmp q1 q2 = comparing prop q1 q2

        minQuote :: QuoteData
        minQuote = minimumBy cmp quotes

        maxQuote :: QuoteData
        maxQuote = maximumBy cmp quotes

        days :: Int
        days = fromIntegral $ abs $ diffDays (day minQuote) (day maxQuote)

statInfo :: (Functor t, Foldable t) => t QuoteData -> [StatEntry]
statInfo quotes = fmap qFieldStatInfo [minBound .. maxBound]
    where
        decimalPlacesbyQField :: QField -> Integer
        decimalPlacesbyQField Volume = 0
        decimalPlacesbyQField _ = decimalPlacesFloating

        qFieldStatInfo :: QField -> StatEntry
        qFieldStatInfo qfield =
            let get :: QuoteData -> Double
                get qd = field2Fun qfield qd

                (mn, mx, daysbetweenMinMax) = computeMinMaxDays get quotes

                decPlaces :: Integer
                decPlaces = decimalPlacesbyQField qfield

                meanVal :: StatValue
                meanVal = StatValue (fromIntegral decimalPlacesFloating) (mean $ fmap get quotes)

                minVal :: StatValue
                minVal = StatValue (fromIntegral decPlaces) mn

                maxVal :: StatValue
                maxVal = StatValue (fromIntegral decPlaces) mx
             in StatEntry {..}