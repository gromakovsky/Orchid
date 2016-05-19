{-# LANGUAGE ScopedTypeVariables #-}

-- | Compiler specification.

module Test.Orchid.CompilerSpec
       ( spec
       ) where

import           Data.String.Conversions (convertString)
import           Data.Text               (Text)
import           System.FilePath         ((</>))
import           System.IO.Temp          (withSystemTempDirectory)
import           Test.Hspec              (Spec, describe, it, shouldBe,
                                          shouldThrow)
import           Turtle                  (procStrict)

import           Orchid.Compiler         (compileStr)
import           Orchid.Error            (CodegenException)

import           Test.Orchid.Data

spec :: Spec
spec =
    describe "Compiler" $ do
        describe "compileStr" $ do
            it "Compiles code in Orchid language" $ do
                checkOutput "factorial_io.orc" factorial_ioInput "6\n" "720\n"
                checkOutput "class.orc" classInput "" "42\n"
                checkOutput "error.orc" errorInput "" "Error occurred\n"
                checkOutput "rectangle.orc" rectangleInput "" "2\n1\n"
                checkOutput "private_var_inside.orc" private_var_insideInput "" "42\n"
                checkOutput "class_method_inside.orc" class_method_insideInput "" "2\n1\n"
            it "Reports error for invalid code" $ do
                expectError "type_error.orc" type_errorInput
                expectError "private_var_outside.orc" private_var_outsideInput
  where
    checkOutput prName prSource prInput prOutput = do
        (`shouldBe` prOutput) =<< programOutput prName prSource prInput
    expectError prName prSource = do
        programOutput prName prSource undefined `shouldThrow`
            (\(_ :: CodegenException) -> True)

programOutput :: String -> Text -> Text -> IO Text
programOutput programName programSource programInput =
    withSystemTempDirectory "patak" cb
  where
    cb dir = do
        putStrLn programName
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
