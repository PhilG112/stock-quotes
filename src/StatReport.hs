module StatReport where

import Data.Foldable (maximumBy, minimumBy)
import Data.Ord
import Data.Time
import QuoteData (QField, QuoteData (day))

decimalPlacesFloating :: Integer
decimalPlacesFloating = 2

data StatValue = StatValue
    { decimalPlaces :: Int
    , value :: Double
    }

data StatEntry = StatEntry
    { qField :: QField
    , meanVal :: StatValue
    , minVal :: StatValue
    , maxVal :: StatEntry
    , daysBetweenMinMax :: Int
    }

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
statInfo quotes = _