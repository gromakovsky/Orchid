{-# LANGUAGE DeriveGeneric #-}

-- | Command line options for orchid.

module Options
       ( Options (..)
       , getOptions
       ) where

import qualified Options.Applicative as Opts

data Options = Options
    { optInputFile   :: FilePath
    , optOutputFile  :: FilePath
    , optDumpAST     :: Bool
    , optDumpTokens  :: Bool
    , optNotOptimize :: Bool
    } deriving (Show)

parser :: Opts.Parser Options
parser =
    Options <$>
    Opts.strArgument
        (mconcat [Opts.metavar "INPUT", Opts.help "Path to input file"]) <*>
    Opts.strOption
        (mconcat
             [ Opts.short 'o'
             , Opts.long "output"
             , Opts.metavar "OUTPUT"
             , Opts.help "Path to output file"
             , Opts.value "a.ll"
             , Opts.showDefault]) <*>
    Opts.switch (mconcat [Opts.short 'a', Opts.long "ast"]) <*>
    Opts.switch (mconcat [Opts.short 't', Opts.long "tokens"]) <*>
    Opts.switch (mconcat [Opts.long "not-optimize"])

getOptions :: IO Options
getOptions =
    Opts.execParser $
    Opts.info (Opts.helper <*> parser) $
    mconcat [Opts.fullDesc, Opts.progDesc "Orchid compiler"]
