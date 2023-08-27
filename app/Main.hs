module Main where

import qualified MyLib (someFunc)
import QuoteData ( QuoteData )
import qualified Data.ByteString.Lazy as BL (readFile, writeFile)
import Data.Either ()
import Data.Foldable ( Foldable(toList) )
import Data.Csv (decodeByName)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc

readQuotes :: FilePath -> IO [QuoteData]
readQuotes fpath = do
  csvData <- BL.readFile fpath
  case decodeByName csvData of
    Left err -> error err
    Right (_, quotes) -> pure (toList quotes)