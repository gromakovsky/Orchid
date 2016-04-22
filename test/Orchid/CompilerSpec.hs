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

factorial_ioInput
    :: IsString s
    => s
factorial_ioInput = $(embedStringFile "test/data/factorial_io.orc")

classInput
    :: IsString s
    => s
classInput = $(embedStringFile "test/data/class.orc")

errorInput
    :: IsString s
    => s
errorInput = $(embedStringFile "test/data/error.orc")

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
            it "Properly compiles factorial_io.orc" $
                shouldBe "720\n" =<< programOutput factorial_ioInput "6\n"
            it "Properly compiles class.orc" $
                shouldBe "42\n" =<< programOutput classInput ""
            it "Properly compiles error.orc" $
                shouldBe "Error occurred\n" =<< programOutput errorInput ""
