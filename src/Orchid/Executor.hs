-- | Compile and execute programs in Orchid language.

module Orchid.Executor
       ( executeProgram
       , Optimization (..)
       , Optimizations
       ) where

import           Data.String.Conversions (convertString)
import           Data.Text               (Text)
import           System.FilePath         ((</>))
import           System.IO.Temp          (withSystemTempDirectory)
import           Turtle                  (procStrict)

import           Orchid.Compiler         (Optimization (..), Optimizations,
                                          compileStr)

executeProgram :: Optimizations -> Text -> Text -> IO Text
executeProgram optimizations programSource programInput =
    withSystemTempDirectory "patak" cb
  where
    cb dir = do
        compileStr optimizations programSource $ dir </> "a.ll"
        () <$
            procStrict
                "llvm-as"
                [ "-o"
                , convertString $ dir </> "a.bc"
                , convertString $ dir </> "a.ll"]
                mempty
        snd <$>
            (procStrict "lli" [convertString $ dir </> "a.ll"] $
             pure programInput)
