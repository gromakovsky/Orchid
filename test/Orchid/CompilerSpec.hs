{-# LANGUAGE TemplateHaskell #-}

-- | Compiler specification.

module Orchid.CompilerSpec where

import           Data.FileEmbed          (embedStringFile)
import           Data.String             (IsString)
import           Data.String.Conversions (convertString)
import           Data.Text               (Text)
import           System.FilePath         ((</>))
import           System.IO.Temp          (withSystemTempDirectory)
import           Test.Hspec              (Spec, describe, it, shouldBe)
import           Turtle                  (procStrict)

import           Orchid.Compiler         (compileStr)

factorial_io
    :: IsString s
    => s
factorial_io = $(embedStringFile "test/data/factorial_io.orc")

programOutput :: Text -> Text -> IO Text
programOutput programSource programInput = withSystemTempDirectory "patak" cb
  where
    cb dir = do
        compileStr programSource $ dir </> "a.ll"
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

spec :: Spec
spec =
    describe "Compiler" $ do
        describe "compileStr" $ do
            it "compiles input string and dumps result to output file as textual LLVM IR" $ do
                factorialOutput <- programOutput factorial_io "6\n"
                factorialOutput `shouldBe` "720\n"
