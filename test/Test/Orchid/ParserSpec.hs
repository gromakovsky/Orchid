-- | Parser specification.

module Test.Orchid.ParserSpec
       ( spec
       ) where

import           Data.Either   (isLeft)
import           Data.Text     (Text, unlines)
import           Prelude       hiding (unlines)
import           Test.Hspec    (Spec, describe, it, shouldBe, shouldSatisfy)

import           Orchid.Parser (parseInput)
import qualified Orchid.Types  as T

identifierExpr :: T.Identifier -> T.Expr
identifierExpr = T.EAtom . T.AEAtom . T.AIdentifier

validInput1 :: Text
validInput1 = "1 + 2 * 3\n"

validRes1 :: [T.Stmt]
validRes1 = [T.SSimple $ T.SimpleStmt [T.SSExpr (T.ExprStmt Nothing expr)]]
  where
    expr = T.EBinary T.BinPlus e1 e23
    e1 = T.EAtom $ T.AEAtom $ T.ANumber 1
    e23 = T.EBinary T.BinMult e2 e3
    e2 = T.EAtom $ T.AEAtom $ T.ANumber 2
    e3 = T.EAtom $ T.AEAtom $ T.ANumber 3

validInput2 :: Text
validInput2 = unlines ["if a:", "  t = print(b)", "else:", "  bool t = a", ""]

validRes2 :: [T.Stmt]
validRes2 = [T.SCompound $ T.CSIf $ T.IfStmt aExpr trueSuite (Just falseSuite)]
  where
    aExpr = T.EAtom $ T.AEAtom $ T.AIdentifier "a"
    trueSuite = T.Suite [T.SSimple $ T.SimpleStmt [T.SSExpr trueExprStmt]]
    trueExprStmt = T.ExprStmt (Just "t") trueExpr
    trueExpr = T.EAtom $ T.AECall (T.AEAtom $ T.AIdentifier "print") [printArg]
    printArg = T.EAtom $ T.AEAtom $ T.AIdentifier "b"
    falseSuite = T.Suite [T.SSimple $ T.SimpleStmt [T.SSDecl falseDeclStmt]]
    falseDeclStmt = T.DeclStmt "bool" "t" falseExpr
    falseExpr = T.EAtom $ T.AEAtom $ T.AIdentifier "a"

validInput3 :: Text
validInput3 =
    unlines
        [ "class Point:"
        , "  public int64 x = 0"
        , "  public int64 y = 0"
        , "  public def print():"
        , "    magic(x, y,)"]

validRes3 :: [T.Stmt]
validRes3 = [T.SCompound $ T.CSClass classDef]
  where
    classDef =
        T.ClassDef
        { clsName = "Point"
        , clsParent = Nothing
        , clsBody = T.ClassSuite classBody
        }
    classBody = [xDeclS, yDeclS, printDeclS]
    xDeclS = T.ClassStmt T.AMPublic $ Right xDecl
    xDecl = T.DeclStmt "int64" "x" zeroExpr
    yDeclS = T.ClassStmt T.AMPublic $ Right yDecl
    yDecl = T.DeclStmt "int64" "y" zeroExpr
    zeroExpr = T.EAtom $ T.AEAtom $ T.ANumber 0
    printDeclS = T.ClassStmt T.AMPublic $ Left printDecl
    printDecl =
        T.FuncDef "print" [] Nothing $
        T.Suite
            [T.SSimple $ T.SimpleStmt [T.SSExpr $ T.ExprStmt Nothing magicExpr]]
    magicExpr =
        T.EAtom $
        T.AECall (T.AEAtom $ T.AIdentifier "magic") $
        [identifierExpr "x", identifierExpr "y"]

validInput4 :: Text
validInput4 = "a = p.print()\n"

validRes4 :: [T.Stmt]
validRes4 = [T.SSimple $ T.SimpleStmt [T.SSExpr exprStmt]]
  where
    exprStmt = T.ExprStmt (Just "a") printExpr
    printExpr = T.EAtom $ T.AECall f []
    f = T.AEAccess (T.AEAtom $ T.AIdentifier "p") "print"

spec :: Spec
spec =
    describe "Parser" $ do
        describe "parseInput" $ do
            it "parses valid input into AST represented as Input data type" $ do
                mapM_ checkValid validInputs
            it "reports error for invalid input" $ do
                mapM_ (\i -> parse i `shouldSatisfy` isLeft) invalidInputs
  where
    sourceName = "test-suite"
    parse = parseInput sourceName
    checkValid (inp, expected) = parse inp `shouldBe` Right (T.Input expected)
    validInputs =
        [ (validInput1, validRes1)
        , (validInput2, validRes2)
        , (validInput3, validRes3)
        , (validInput4, validRes4)]
    invalidInputs = [invalid1, invalid2]
    invalid1 = "def f():\n  pass\n pass"
    invalid2 = "1 + / 2"
