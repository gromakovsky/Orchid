{-# LANGUAGE TemplateHaskell #-}

-- | Compiler wraps parser, translator and other stuff into one
-- module.

module Orchid.Compiler
       ( CompilerOptions (..)
       , CompilerExtra (..)
       , compileStr
       , compile
       ) where

import           Control.Exception (catch)
import           Data.FileEmbed    (embedStringFile, makeRelativeToProject)
import           Data.String       (IsString)
import           Data.Text         (Text)
import qualified Data.Text.IO      as TIO

import           Orchid.Error      (CodegenException (..))
import           Orchid.Lexer      (tokenizeInputFile)
import           Orchid.Parser     (parseInput, parseInputFile)
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

orchidPreludeStr
    :: IsString s
    => s
orchidPreludeStr =
    $(makeRelativeToProject "src/prelude.orc" >>= embedStringFile)

orchidPrelude :: Input
orchidPrelude =
    either (error "Fatal error: failed to parse prelude") id $
    parseInput "prelude" orchidPreludeStr

compileStr :: Text -> FilePath -> IO ()
compileStr inputText outFp =
    either print (doTranslate outFp) $ parseInput "<text>" inputText

doTranslate :: FilePath -> Input -> IO ()
doTranslate outFp input =
    translateToFile outFp (mconcat [orchidPrelude, input]) `catch`
    handleCodegenException
  where
    handleCodegenException (CodegenException t) = TIO.putStrLn t

compile :: CompilerOptions -> IO CompilerExtra
compile CompilerOptions{..} = do
    input <- parseInputFile coInputFile
    doTranslate coOutputFile input
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
