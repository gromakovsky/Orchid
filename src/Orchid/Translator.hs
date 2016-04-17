{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TupleSections          #-}

-- | Translator transforms AST into LLVM representation.

module Orchid.Translator
       ( translate
       , translateToFile
       ) where

import           Control.Exception       (throwIO)
import           Control.Monad           ((<=<))
import           Control.Monad.Except    (ExceptT, runExceptT, throwError)
import qualified Data.Map                as M
import           Data.Monoid             ((<>))
import           Data.String.Conversions (convertString)
import qualified LLVM.General            as G
import qualified LLVM.General.AST        as AST
import           LLVM.General.Context    (withContext)

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
    m0 = C.emptyModule mName
    mFinalEither = C.execLLVM m0 $ toLLVM inp

translateToFile :: FilePath -> OT.Input -> IO ()
translateToFile fp input =
    translate fp input (handleError . G.writeLLVMAssemblyToFile (G.File fp))

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

predefinedTypes :: M.Map OT.Identifier C.Type
predefinedTypes =
    M.fromList [("int32", C.int32), ("int64", C.int64), ("bool", C.bool)]

lookupType :: OT.Identifier -> C.LLVM C.Type
lookupType t =
    maybe (C.throwCodegenError $ "unknown type: " <> t) return $
    M.lookup t predefinedTypes

convertTypedArg :: OT.TypedArgument -> C.LLVM (C.Type, C.Name)
convertTypedArg OT.TypedArgument{..} =
    (, C.Name (convertString taName)) <$> lookupType taType

--------------------------------------------------------------------------------
-- ToLLVM
--------------------------------------------------------------------------------

class ToLLVM a  where
    toLLVM :: a -> C.LLVM ()

instance ToLLVM OT.Input where
    toLLVM = mapM_ toLLVM . OT.getInput

instance ToLLVM OT.Stmt where
    toLLVM (OT.SSimple s) = toLLVM s
    toLLVM (OT.SCompound s) = toLLVM s

-- TODO: report error
instance ToLLVM OT.SimpleStmt where
    toLLVM (OT.SimpleStmt smallStmts) = mapM_ toLLVM smallStmts

instance ToLLVM OT.SmallStmt where
    toLLVM _ = return ()

instance ToLLVM OT.CompoundStmt where
    toLLVM (OT.CSIf _) =
        C.throwCodegenError "'if' is not a valid top-level statement"
    toLLVM (OT.CSWhile _) =
        C.throwCodegenError "'while' is not a valid top-level statement"
    toLLVM (OT.CSFunc s) = toLLVM s

instance ToLLVM OT.FuncDef where
    toLLVM OT.FuncDef{..} = do
        ret <- maybe (return C.void) lookupType funcRet
        args <- mapM convertTypedArg funcArgs
        either throwError (C.addFuncDef ret (convertString funcName) args) $
            bodyEither args
      where
        bodyEither args = C.execCodegen (bodyCodegen args) >>= C.createBlocks
        bodyCodegen args = do
            mapM_ codegenArgument args
            toCodegen funcBody
        codegenArgument (argType,argName) = do
            var <- C.alloca argType
            () <$ (C.store var $ AST.LocalReference argType argName)
            C.assign (fromName argName) var
        fromName (AST.Name s) = s
        fromName _ = error "fromName failed"


--------------------------------------------------------------------------------
-- ToCodegen
--------------------------------------------------------------------------------

todo :: C.Codegen a
todo = C.throwCodegenError "TODO"

class ToCodegen a r | a -> r where
    toCodegen :: a -> C.Codegen r

instance ToCodegen OT.Stmt () where
    toCodegen (OT.SSimple s) = toCodegen s
    toCodegen (OT.SCompound s) = toCodegen s

instance ToCodegen OT.SimpleStmt () where
    toCodegen (OT.SimpleStmt smallStmts) = mapM_ toCodegen smallStmts

instance ToCodegen OT.SmallStmt () where
    toCodegen (OT.SSDecl _) = todo
    toCodegen (OT.SSExpr e) = toCodegen e
    toCodegen OT.SSPass = return ()
    toCodegen (OT.SSFlow fs) = toCodegen fs

instance ToCodegen OT.ExprStmt () where
    toCodegen OT.ExprStmt {..} = () <$ toCodegen esExpr  -- TODO

instance ToCodegen OT.FlowStmt () where
    toCodegen (OT.FSReturn rs) = toCodegen rs

instance ToCodegen OT.ReturnStmt () where
    toCodegen (OT.ReturnStmt me) =
        () <$ maybe (C.ret Nothing) (C.ret . Just <=< toCodegen) me

instance ToCodegen OT.Expr AST.Operand where
    toCodegen (OT.EUnary _ _) = todo
    toCodegen (OT.EBinary OT.BinPlus a b) = do
        op1 <- toCodegen a
        op2 <- toCodegen b
        C.add op1 op2
    toCodegen (OT.EBinary _ _ _) = todo
    toCodegen (OT.EAtom a) = toCodegen a

instance ToCodegen OT.AtomExpr AST.Operand where
    toCodegen (OT.AEAtom a) = toCodegen a
    toCodegen (OT.AECall _ _) = todo

instance ToCodegen OT.Atom AST.Operand where
    toCodegen (OT.AExpr e) = toCodegen e
    toCodegen (OT.AIdentifier v) = C.getVar (convertString v) >>= C.load
    toCodegen (OT.ANumber n) = pure $ C.constInt64 n
    toCodegen (OT.ABool b) = pure $ C.constBool b

instance ToCodegen OT.CompoundStmt () where
    toCodegen (OT.CSIf _) = todo
    toCodegen (OT.CSWhile _) = todo
    toCodegen (OT.CSFunc _) =
        C.throwCodegenError "nested functions are not supported"

instance ToCodegen OT.Suite () where
    toCodegen = mapM_ toCodegen . OT.getSuite
