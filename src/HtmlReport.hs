{-# LANGUAGE OverloadedStrings #-}

module HtmlReport where

import Colonnade (Colonnade, Headed, headed)
import Control.Monad (unless)
import Data.Foldable (Foldable (null), traverse_)
import Fmt (Buildable, pretty)
import QuoteData (QuoteData (close, day, high, low, open, volume))
import StatReport (StatEntry (daysBetweenMinMax, maxVal, meanVal, minVal, qField), showPrice)
import Text.Blaze.Colonnade (encodeHtmlTable)
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Text.Blaze.Html5 as H (Html, ToValue (toValue), body, docTypeHtml, h1, head, i, img, string, style, text, title, (!))
import Text.Blaze.Html5.Attributes (src)

individualFmt :: Buildable a => a -> Html
individualFmt t = text $ pretty t

colStats :: Colonnade Headed StatEntry Html
colStats =
    mconcat
        [ headed "Quote field" (\sa -> i $ string $ show $ qField sa)
        , headed "Mean" (\sa -> individualFmt $ meanVal sa)
        , headed "Min" (\sa -> individualFmt $ minVal sa)
        , headed "Max" (\sa -> individualFmt $ maxVal sa)
        , headed "Days between min/max" (\sa -> individualFmt $ daysBetweenMinMax sa)
        ]

colData :: Colonnade Headed QuoteData Html
colData =
    mconcat
        [ headed "Day" (individualFmt . day)
        , headed "Open" (individualFmt . showPrice . open)
        , headed "Close" (individualFmt . showPrice . close)
        , headed "High" (individualFmt . showPrice . high)
        , headed "Low" (individualFmt . showPrice . low)
        , headed "Volume" (individualFmt . volume)
        ]

htmlReport ::
    (Functor t, Foldable t) =>
    String ->
    t QuoteData ->
    [StatEntry] ->
    [FilePath] ->
    String
htmlReport docTitle quotes statEntries images = renderHtml $ docTypeHtml $ do
    H.head $ do
        title $ string docTitle
        style tableStyle
    body $ do
        unless (Data.Foldable.null images) $ do
            h1 "Charts"
            traverse_ (\im -> (img !) $ src $  toValue im) images
        h1 "Stats report"
        encodeHtmlTable mempty colStats statEntries

        h1 "Stock Quotes data"
        encodeHtmlTable mempty colData quotes
    where
        tableStyle =
            "table {border-collapse: collapse}"
                <> "td, th {border: 1px solid black; padding: 5px}"