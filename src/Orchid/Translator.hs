{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}

-- | Translator transforms AST into LLVM representation.

module Orchid.Translator
       ( translate
       , translateToFile
       ) where

import           Control.Exception       (throwIO)
import           Control.Monad           (join, (<=<))
import           Control.Monad.Except    (ExceptT, runExceptT)
import qualified Data.Map                as M
import           Data.Monoid             ((<>))
import           Data.String.Conversions (convertString)
import qualified LLVM.General            as G
import qualified LLVM.General.AST        as AST
import           LLVM.General.Context    (withContext)
import           Serokell.Util           (formatSingle')

import qualified Orchid.Codegen          as C
import           Orchid.Error            (LowLevelException (LowLevelException))
import qualified Orchid.Types            as OT

type ModuleName = String

--------------------------------------------------------------------------------
-- Exported
--------------------------------------------------------------------------------

handleError :: ExceptT String IO a -> IO a
handleError =
    either (throwIO . LowLevelException . convertString) return <=< runExceptT

translate :: ModuleName -> OT.Input -> (G.Module -> IO a) -> IO a
translate mName inp continuation =
    withContext $
    \context ->
         either
             throwIO
             (\m ->
                   handleError $ G.withModuleFromAST context m continuation)
             mFinalEither
  where
    initialState = C.emptyLLVM mName
    mFinalEither = C.execLLVM initialState $ C.toLLVM inp

translateToFile :: FilePath -> OT.Input -> IO ()
translateToFile fp input =
    translate fp input (handleError . G.writeLLVMAssemblyToFile (G.File fp))

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

predefinedTypes :: M.Map OT.Identifier C.Type
predefinedTypes =
    M.fromList [("int64", C.int64), ("bool", C.bool)]

lookupType :: OT.Identifier -> C.LLVM C.Type
lookupType t =
    maybe (C.throwCodegenError $ "unknown type: " <> t) return $
    M.lookup t predefinedTypes

convertTypedArg :: OT.TypedArgument -> C.LLVM (C.Type, C.Name)
convertTypedArg OT.TypedArgument{..} =
    (, C.Name (convertString taName)) <$> lookupType taType

--------------------------------------------------------------------------------
-- C.ToLLVM
--------------------------------------------------------------------------------

instance C.ToLLVM OT.Input where
    toLLVM = mapM_ C.toLLVM . OT.getInput

instance C.ToLLVM OT.Stmt where
    toLLVM (OT.SSimple s) = C.toLLVM s
    toLLVM (OT.SCompound s) = C.toLLVM s

instance C.ToLLVM OT.SimpleStmt where
    toLLVM (OT.SimpleStmt smallStmts) = mapM_ C.toLLVM smallStmts

instance C.ToLLVM OT.SmallStmt where
    toLLVM (OT.SSExpr _) =
        C.throwCodegenError "top-level expression statements are not allowed"
    toLLVM (OT.SSDecl ds) = C.toLLVM ds
    toLLVM OT.SSPass = return ()
    toLLVM (OT.SSFlow _) =
        C.throwCodegenError "top-level flow statements are not allowed"

instance C.ToLLVM OT.DeclStmt where
    toLLVM OT.DeclStmt{..} =
        join $
        C.addGlobalVariable <$> lookupType dsType <*>
        pure (convertString dsVar) <*>
        pure dsExpr

instance C.ToLLVM OT.CompoundStmt where
    toLLVM (OT.CSIf _) =
        C.throwCodegenError "'if' is not a valid top-level statement"
    toLLVM (OT.CSWhile _) =
        C.throwCodegenError "'while' is not a valid top-level statement"
    toLLVM (OT.CSFunc s) = C.toLLVM s

instance C.ToLLVM OT.FuncDef where
    toLLVM OT.FuncDef{..} = do
        ret <- maybe (return C.void) lookupType funcRet
        args <- mapM convertTypedArg funcArgs
        C.addFuncDef ret (convertString funcName) args $ bodyCodegen args
      where
        bodyCodegen args = do
            mapM_ codegenArgument args
            C.toCodegen funcBody
        codegenArgument (argType,argName) = do
            var <- C.alloca argType
            () <$ (C.store var $ AST.LocalReference argType argName)
            C.assignLocal (fromName argName) var
        fromName (AST.Name s) = s
        fromName _ = error "fromName failed"

--------------------------------------------------------------------------------
-- C.ToCodegen
--------------------------------------------------------------------------------

todo :: C.Codegen a
todo = C.throwCodegenError "TODO"

instance C.ToCodegen OT.Stmt () where
    toCodegen (OT.SSimple s) = C.toCodegen s
    toCodegen (OT.SCompound s) = C.toCodegen s

instance C.ToCodegen OT.SimpleStmt () where
    toCodegen (OT.SimpleStmt smallStmts) = mapM_ C.toCodegen smallStmts

instance C.ToCodegen OT.SmallStmt () where
    toCodegen (OT.SSDecl _) = todo
    toCodegen (OT.SSExpr e) = C.toCodegen e
    toCodegen OT.SSPass = return ()
    toCodegen (OT.SSFlow fs) = C.toCodegen fs

instance C.ToCodegen OT.ExprStmt () where
    toCodegen OT.ExprStmt {..} = () <$ C.toCodegen esExpr  -- TODO

instance C.ToCodegen OT.FlowStmt () where
    toCodegen (OT.FSReturn rs) = C.toCodegen rs

instance C.ToCodegen OT.ReturnStmt () where
    toCodegen (OT.ReturnStmt me) =
        () <$ maybe (C.ret Nothing) (C.ret . Just <=< C.toCodegen) me

instance C.ToCodegen OT.Expr AST.Operand where
    toCodegen (OT.EUnary uOp a) = C.toCodegen a >>= convertUnOp uOp
      where
        convertUnOp OT.UnaryPlus = pure
        convertUnOp OT.UnaryMinus = C.neg
        convertUnOp OT.UnaryNot = C.not
    toCodegen (OT.EBinary bOp a b) = do
        op1 <- C.toCodegen a
        op2 <- C.toCodegen b
        convertBinOp bOp op1 op2
      where
        convertBinOp OT.BinOr = C.or
        convertBinOp OT.BinAnd = C.and
        convertBinOp OT.BinLT = C.lessThan
        convertBinOp OT.BinGT = C.greaterThan
        convertBinOp OT.BinEQ = C.equal
        convertBinOp OT.BinLE = C.lessOrEqual
        convertBinOp OT.BinNE = C.notEqual
        convertBinOp OT.BinGE = C.greaterOrEqual
        convertBinOp OT.BinPlus = C.add
        convertBinOp OT.BinMinus = C.sub
        convertBinOp OT.BinMult = C.mul
        convertBinOp OT.BinDiv = C.div
        convertBinOp OT.BinMod = C.mod
        convertBinOp OT.BinPower =
            \a' b' ->
                 do f <- C.toCodegen (OT.AIdentifier "stdPower")
                    C.call f [a', b']
    toCodegen (OT.EAtom a) = C.toCodegen a

instance C.ToCodegen OT.AtomExpr AST.Operand where
    toCodegen (OT.AEAtom a) = C.toCodegen a
    toCodegen (OT.AECall f exprs) =
        join $ C.call <$> C.toCodegen f <*> mapM C.toCodegen exprs

instance C.ToCodegen OT.Atom AST.Operand where
    toCodegen (OT.AExpr e) = C.toCodegen e
    toCodegen (OT.AIdentifier v) = C.getVar $ convertString v
    toCodegen (OT.ANumber n) = AST.ConstantOperand <$> C.toConstant n
    toCodegen (OT.ABool b) = AST.ConstantOperand <$> C.toConstant b

instance C.ToCodegen OT.CompoundStmt () where
    toCodegen (OT.CSIf _) = todo
    toCodegen (OT.CSWhile _) = todo
    toCodegen (OT.CSFunc _) =
        C.throwCodegenError "nested functions are not supported"

instance C.ToCodegen OT.Suite () where
    toCodegen = mapM_ C.toCodegen . OT.getSuite

--------------------------------------------------------------------------------
-- C.ToConstant
--------------------------------------------------------------------------------

instance C.ToConstant OT.Expr where
    toConstant (OT.EAtom a) = C.toConstant a
    toConstant _ = C.throwCodegenError "TODO"

instance C.ToConstant OT.AtomExpr where
    toConstant (OT.AEAtom a) = C.toConstant a
    toConstant (OT.AECall _ _) =
        C.throwCodegenError "function call can't be used as constant"

instance C.ToConstant OT.Atom where
    toConstant (OT.AExpr e) = C.toConstant e
    toConstant (OT.AIdentifier name) =
        C.throwCodegenError $
        formatSingle' "identifier ({}) can't be used as constant" name
    toConstant (OT.ANumber n) = C.toConstant n
    toConstant (OT.ABool b) = C.toConstant b
