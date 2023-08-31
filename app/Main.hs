{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (when, unless)
import qualified Data.ByteString.Lazy as BL (readFile, writeFile)
import Data.Csv (decodeByName)
import Data.Text (unpack)

import QuoteData ( QuoteData )
import Charts ( plotChart )
import StatReport ( textReport, statInfo )
import HtmlReport ( htmlReport )
import Params ( Params(..), cmdLineParser )

main :: IO ()
main = cmdLineParser >>= work

work :: Params -> IO ()
work params = do
    csvData <- BL.readFile (fname params)
    case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, quotes) -> generateReports params quotes

generateReports :: (Functor t, Foldable t) => Params -> t QuoteData -> IO ()
generateReports Params {..} quotes = do
    unless silent $ putStr textRpt
    when chart $ plotChart title quotes chartFname
    saveHtml htmlFile htmlRpt
    where
        statInfo' = statInfo quotes
        textRpt = textReport statInfo'
        htmlRpt = htmlReport title quotes statInfo' [chartFname | chart]

        withCompany prefix = maybe mempty (prefix <>) company
        chartFname = unpack $ "chart" <> withCompany "_" <> ".svg"
        title = unpack $ "Historical quote" <> withCompany " for "

        saveHtml Nothing _ = pure ()
        saveHtml (Just f) html = BL.writeFile f html