{-# LANGUAGE ScopedTypeVariables #-}

-- | Overall compiler specification.

module Test.Orchid.CompilerSpec
       ( spec
       ) where

import           Data.Text          (Text)
import           Test.Hspec         (Spec, context, describe, it, shouldBe,
                                     shouldThrow)

import           Serokell.Util.Text (listBuilder, show')

import           Orchid.Error       (CodegenException)
import           Orchid.Executor    (Optimization (..), Optimizations,
                                     executeProgram)

import           Test.Orchid.Data

optimizeTailRec :: Optimizations
optimizeTailRec = [TailRecursionOptimization]

spec :: Spec
spec =
    describe "Orchid compiler" $ do
        context "Properly compiles well-formed code" $ do
            genSpecNumbers "factorial_io.orc" factorial_ioSource "6\n" [720]
            genSpecNumbers "class.orc" classSource "" [42]
            genSpecErrorOutput "error.orc" errorSource ""
            genSpecNumbers "global_var.orc" global_varSource "" [1, 2]
            genSpecNumbers "rectangle.orc" rectangleSource "" [2, 1]
            genSpecNumbers "private_var_inside.orc" private_var_insideSource "" [42]
            genSpecNumbers "class_method_inside.orc" class_method_insideSource "" [2, 1]
            genSpecNumbers "private_method_inside.orc" private_method_insideSource "" [2, 1]
            genSpecNumbers "inheritance.orc" inheritanceSource "" [10, 25]
            genSpecNumbers "pointer.orc" pointerSource "" [10]
            genSpecNumbers "shape.orc" shapeSource "" [0, 12, 3]
            genSpecNumbers "virtual.orc" virtualSource "" [1, 2, 2, 22, 23]
            genSpecNumbers "new.orc" newSource "" [1, 2]
            genSpecNumbers "new_class.orc" new_classSource "" [10, 1, 2, 0]
            genSpecNumbers "fib.orc" fibSource "" [8]
            genSpecNumbers "factorial.orc" factorialSource "" [40320]
            genSpecNumbers "mutually_recursive.orc" mutually_recursiveSource "" [37]
        context "Reports error for semantically invalid code" $ do
            genSpecCodegenError "type_error.orc" type_errorSource
            genSpecCodegenError "private_var_outside.orc" private_var_outsideSource
            genSpecCodegenError "private_method_outside.orc" private_method_outsideSource

genSpec :: String -> Text -> Text -> Text -> Spec
genSpec prName prSource prInput prOutput =
    it ("Properly compiles " ++ prName) $
    expectOutput prSource prInput prOutput

genSpecNumbers :: String -> Text -> Text -> [Int] -> Spec
genSpecNumbers n src inp = genSpec n src inp . show' . listBuilder emptyText newline newline
  where
    emptyText :: Text
    emptyText = ""
    newline :: Text
    newline = "\n"

genSpecErrorOutput :: String -> Text -> Text -> Spec
genSpecErrorOutput n src inp = genSpec n src inp "Error occurred\n"

genSpecCodegenError :: String -> Text -> Spec
genSpecCodegenError prName prSource =
    it ("Throws CodegenException in compiling " ++ prName) $
    expectCodegenError prSource

expectOutput :: Text -> Text -> Text -> IO ()
expectOutput prSource prInput prOutput =
    (`shouldBe` prOutput) =<< executeProgram optimizeTailRec prSource prInput

expectCodegenError :: Text -> IO ()
expectCodegenError prSource =
    executeProgram optimizeTailRec prSource undefined `shouldThrow`
    (\(_ :: CodegenException) ->
          True)
