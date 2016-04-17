{-# LANGUAGE DeriveGeneric #-}

-- | Command line options for orchid.

module Options
       ( Options (..)
       , getOptions
       ) where

import qualified Options.Applicative as Opts

data Options = Options
    { optInputFile :: FilePath
    } deriving (Show)

parser :: Opts.Parser Options
parser =
    Options <$>
    Opts.strArgument
        (mconcat [Opts.metavar "INPUT", Opts.help "Path to input file"])

getOptions :: IO Options
getOptions =
    Opts.execParser $
    Opts.info (Opts.helper <*> parser) $
    mconcat [Opts.fullDesc, Opts.progDesc "Orchid compiler"]
