-- | Compiler wraps parser, translator and other stuff into one
-- module.

module Orchid.Compiler
       ( CompilerOptions (..)
       , CompilerExtra (..)
       , compile
       ) where

import           Orchid.Lexer      (tokenizeInputFile)
import           Orchid.Parser     (parseInputFile)
import           Orchid.Token      (Token)
import           Orchid.Translator (translateToFile)
import           Orchid.Types      (Input)

data CompilerOptions = CompilerOptions
    { coInputFile    :: FilePath
    , coOutputFile   :: FilePath
    , coReturnTokens :: Bool
    , coReturnTree   :: Bool
    } deriving (Show)

data CompilerExtra = CompilerExtra
    { ceTokens :: Maybe [Token]
    , ceTree   :: Maybe Input
    } deriving (Show)

compile :: CompilerOptions -> IO CompilerExtra
compile CompilerOptions{..} = do
    input <- parseInputFile coInputFile
    translateToFile coOutputFile input
    ceTokens <-
        if coReturnTokens
            then Just <$> tokenizeInputFile coInputFile
            else return Nothing
    return
        CompilerExtra
        { ceTree = if coReturnTree
              then Just input
              else Nothing
        , ..
        }
