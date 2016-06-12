{-# LANGUAGE ScopedTypeVariables #-}

-- | Compiler specification.

module Test.Orchid.CompilerSpec
       ( spec
       ) where

import           Data.Text          (Text)
import           Test.Hspec         (Spec, describe, it, shouldBe, shouldThrow)

import           Serokell.Util.Text (listBuilder, show')

import           Orchid.Error       (CodegenException)
import           Orchid.Executor    (Optimization (..), Optimizations,
                                     executeProgram)

import           Test.Orchid.Data

optimizeTailRec :: Optimizations
optimizeTailRec = [TailRecursionOptimization]

spec :: Spec
spec =
    describe "Compiler" $ do
        it "Compiles code in Orchid language" $ do
            expectNormalOutput "factorial_io.orc" factorial_ioSource "6\n" [720]
            expectNormalOutput "class.orc" classSource "" [42]
            expectErrorOutput "error.orc" errorSource ""
            expectNormalOutput "global_var.orc" global_varSource "" [1, 2]
            expectNormalOutput "rectangle.orc" rectangleSource "" [2, 1]
            expectNormalOutput "private_var_inside.orc" private_var_insideSource "" [42]
            expectNormalOutput "class_method_inside.orc" class_method_insideSource "" [2, 1]
            expectNormalOutput "private_method_inside.orc" private_method_insideSource "" [2, 1]
            expectNormalOutput "inheritance.orc" inheritanceSource "" [10, 25]
            expectNormalOutput "pointer.orc" pointerSource "" [10]
            expectNormalOutput "shape.orc" shapeSource "" [0, 12, 3]
            expectNormalOutput "virtual.orc" virtualSource "" [1, 2, 2, 22, 23]
            expectNormalOutput "new.orc" newSource "" [1, 2]
            expectNormalOutput "new_class.orc" new_classSource "" [10, 1, 2, 0]
            expectNormalOutput "fib.orc" fibSource "" [8]
            expectNormalOutput "factorial.orc" factorialSource "" [40320]
            expectNormalOutput "mutually_recursive.orc" mutually_recursiveSource "" [37]
        it "Reports error for invalid code" $ do
            expectError "type_error.orc" type_errorSource
            expectError "private_var_outside.orc" private_var_outsideSource
            expectError "private_method_outside.orc" private_method_outsideSource
  where
    checkOutput prName prSource prInput prOutput = do
        (`shouldBe` prOutput) =<< executeProgram
            optimizeTailRec prName prSource prInput
    emptyText :: Text
    emptyText = ""
    newline :: Text
    newline = "\n"
    expectNormalOutput :: String -> Text -> Text -> [Int] -> IO ()
    expectNormalOutput prName prSource prInput =
        checkOutput prName prSource prInput .
        show' .
        listBuilder emptyText newline newline
    expectErrorOutput prName prSource prInput =
        checkOutput prName prSource prInput "Error occurred\n"
    expectError prName prSource = do
        executeProgram optimizeTailRec prName prSource undefined `shouldThrow`
            (\(_ :: CodegenException) -> True)
