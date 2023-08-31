module Params where

import Data.Text (Text, strip)
import Options.Applicative
    ( Parser,
      help,
      long,
      metavar,
      optional,
      short,
      strArgument,
      strOption,
      switch, execParser, info, (<**>), helper, fullDesc, progDesc,
    )

data Params = Params
    { fname :: FilePath
    , company :: Maybe Text
    , chart :: Bool
    , htmlFile :: Maybe FilePath
    , silent :: Bool
    }

mkParams :: Parser Params
mkParams =
    Params
        <$> strArgument (metavar "FILE" <> help "CSV file name")
        <*> optional (strip <$> strOption (long "name" <> short 'n' <> help "company name "))
        <*> switch (long "chart" <> short 'c' <> help "generate chart")
        <*> optional (strOption $ long "html" <> metavar "FILE" <> help "generate html report")
        <*> switch (long "silent" <> short 's' <> help "Don't print stats")

cmdLineParser :: IO Params
cmdLineParser = execParser opts
    where
        opts = info (helper <*> mkParams) (fullDesc <> progDesc "Stock quotes data processing")