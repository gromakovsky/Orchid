-- | Compiler specification.

module Test.Orchid.CompilerSpec
       ( spec
       ) where

import           Data.String.Conversions (convertString)
import           Data.Text               (Text)
import           System.FilePath         ((</>))
import           System.IO.Temp          (withSystemTempDirectory)
import           Test.Hspec              (Spec, describe, it, shouldBe)
import           Turtle                  (procStrict)

import           Orchid.Compiler         (compileStr)

import           Test.Orchid.Data

spec :: Spec
spec =
    describe "Compiler" $ do
        describe "compileStr" $ do
            it "Compiles code in Orchid language" $ do
                checkOutput factorial_ioInput "6\n" "720\n"
                checkOutput factorial_ioInput "6\n" "720\n"
                checkOutput classInput "" "42\n"
                checkOutput errorInput "" "Error occurred\n"
  where
    checkOutput prSource prInput prOutput =
        shouldBe prOutput =<< programOutput prSource prInput

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
