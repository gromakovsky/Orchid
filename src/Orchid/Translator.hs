{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

-- | Translator transforms AST into LLVM representation.

module Orchid.Translator
       ( translate
       , translateToFile
       ) where

import           Control.Exception       (throwIO)
import           Control.Monad           (join, unless, when, (<=<))
import           Control.Monad.Except    (ExceptT, runExceptT)
import           Data.FileEmbed          (embedStringFile)
import qualified Data.Map                as M
import           Data.Maybe              (catMaybes)
import           Data.String             (IsString)
import           Data.String.Conversions (convertString)
import qualified LLVM.General            as G
import qualified LLVM.General.AST        as AST
import           LLVM.General.Context    (withContext)
import           Serokell.Util           (formatSingle')

import qualified Orchid.Codegen          as C
import           Orchid.Error            (CodegenException,
                                          FatalError (FatalError),
                                          LowLevelException (LowLevelException))
import qualified Orchid.Types            as OT

type ModuleName = String

--------------------------------------------------------------------------------
-- Exported
--------------------------------------------------------------------------------

translatePure :: ModuleName
              -> AST.Module
              -> OT.Input
              -> Either CodegenException AST.Module
translatePure mName preludeModule inp = C.execLLVM initialState $ C.toLLVM inp
  where
    initialState =
        C.mkModuleState
            mName
            preludeModule
            preludeFunctions
            preludeClasses
            preludeVariables
    preludeFunctions =
        M.fromList
            [ ("stdReadInt", C.FunctionData OT.TInt64 [])
            , ("stdWriteInt", C.FunctionData OT.TVoid [OT.TInt64])
            , ("stdExit", C.FunctionData OT.TVoid [OT.TInt64])]
    preludeClasses = M.empty
    preludeVariables = M.empty

translate :: ModuleName -> OT.Input -> (G.Module -> IO a) -> IO a
translate mName inp continuation =
    withContext $
    \context ->
         handleFatalError $
         G.withModuleFromLLVMAssembly
             context
             (orchidNativePreludeStr :: String) $
         \preludeModuleImpure ->
              do preludeModulePure <- G.moduleAST preludeModuleImpure
                 either throwIO (translateFinish context) $
                     mFinalEither preludeModulePure
  where
    mFinalEither preludeModule = translatePure mName preludeModule inp
    translateFinish context m =
        handleError $ G.withModuleFromAST context m continuation

translateToFile :: FilePath -> OT.Input -> IO ()
translateToFile fp input =
    translate fp input (handleError . G.writeLLVMAssemblyToFile (G.File fp))

--------------------------------------------------------------------------------
-- External
--------------------------------------------------------------------------------

orchidNativePreludeStr
    :: IsString s
    => s
orchidNativePreludeStr = $(embedStringFile "src/native-prelude.ll")

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

handleError :: ExceptT String IO a -> IO a
handleError =
    either (throwIO . LowLevelException . convertString) return <=< runExceptT

handleFatalError :: Show e => ExceptT e IO a -> IO a
handleFatalError =
    either (throwIO . FatalError . formatSingle' "[FATAL] {}" . show) return <=<
    runExceptT

convertTypedArg :: OT.TypedArgument -> C.LLVM (OT.Type, C.Name)
convertTypedArg OT.TypedArgument{..} =
    (, C.Name (convertString taName)) . pointerIfNeeded <$>
    C.lookupType (convertString taType)
  where
    pointerIfNeeded
      | taPointer = OT.TPointer
      | otherwise = id

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
        C.addGlobalVariable <$> C.lookupType (convertString dsType) <*>
        pure (convertString dsVar) <*>
        pure dsExpr

instance C.ToLLVM OT.CompoundStmt where
    toLLVM (OT.CSIf _) =
        C.throwCodegenError "'if' is not a valid top-level statement"
    toLLVM (OT.CSWhile _) =
        C.throwCodegenError "'while' is not a valid top-level statement"
    toLLVM (OT.CSFunc s) = C.toLLVM s
    toLLVM (OT.CSClass s) = C.toLLVM s

instance C.ToLLVM OT.FuncDef where
    toLLVM OT.FuncDef{..} = do
        ret <- maybe (return OT.TVoid) (C.lookupType . convertString) funcRet
        maybe (globalCase ret) (classCase ret) =<< C.getActiveClass
      where
        fromName (AST.Name s) = s
        fromName _ = error "fromName failed"
        globalCase ret = do
            args <- mapM convertTypedArg funcArgs
            C.addFuncDef ret (convertString funcName) args $
                bodyCodegenGlobal args
        bodyCodegenGlobal args = do
            mapM_ codegenArgument args
            C.toCodegen funcBody
        codegenArgument (argType,argName) = do
            addr <- C.alloca argType
            C.store addr $
                ( argType
                , AST.LocalReference (C.orchidTypeToLLVM argType) argName)
            C.addVariable (fromName argName) argType addr
        classCase ret className = do
            ptrType <- C.classPointerType className
            let thisArg = (ptrType, C.thisPtrName)
                mangledFuncName =
                    C.mangleClassMethodName
                        (convertString className)
                        (convertString funcName)
            args <- (thisArg :) <$> mapM convertTypedArg funcArgs
            C.addFuncDef ret mangledFuncName args $ bodyCodegenClass args
        bodyCodegenClass args = do
            let (thisType@(OT.TPointer classType),thisName) = head args
            C.addVariable (fromName thisName) classType $
                ( thisType
                , AST.LocalReference (C.orchidTypeToLLVM thisType) thisName)
            mapM_ codegenArgument $ tail args
            C.toCodegen funcBody

instance C.ToLLVM OT.ClassDef where
    toLLVM OT.ClassDef{..} = do
        C.startClassDef (convertString clsName) (convertString <$> clsParent) =<<
            members
        C.toLLVM clsBody
        C.finishClassDef
      where
        members =
            catMaybes <$>
            (mapM payloadToMember . map OT.csPayload . OT.getClassSuite $
             clsBody)
        payloadToMember (Left _) = pure Nothing
        payloadToMember (Right OT.DeclStmt{..}) =
            Just . (convertString dsVar, ) <$>
            ((,) <$> C.lookupType (convertString dsType) <*>
             C.toConstant dsExpr)

instance C.ToLLVM OT.ClassSuite where
    toLLVM = mapM_ C.toLLVM . OT.getClassSuite

instance C.ToLLVM OT.ClassStmt where
    toLLVM OT.ClassStmt{csAccess = access,csPayload = Left (_, f)} = do
        C.toLLVM f
        when (access == OT.AMPrivate) $
            C.makeFuncPrivate $ convertString $ OT.funcName f
    toLLVM OT.ClassStmt{csAccess = access,csPayload = Right v} = do
        when (access == OT.AMPrivate) $
            C.makeVarPrivate $ convertString $ OT.dsVar v

--------------------------------------------------------------------------------
-- C.ToCodegen
--------------------------------------------------------------------------------

instance C.ToCodegen OT.Stmt () where
    toCodegen (OT.SSimple s) = C.toCodegen s
    toCodegen (OT.SCompound s) = C.toCodegen s

instance C.ToCodegen OT.SimpleStmt () where
    toCodegen (OT.SimpleStmt smallStmts) = mapM_ C.toCodegen smallStmts

instance C.ToCodegen OT.SmallStmt () where
    toCodegen (OT.SSDecl d) = C.toCodegen d
    toCodegen (OT.SSExpr e) = C.toCodegen e
    toCodegen OT.SSPass = return ()
    toCodegen (OT.SSFlow fs) = C.toCodegen fs

instance C.ToCodegen OT.DeclStmt () where
    toCodegen OT.DeclStmt{..} = do
        t <- C.lookupType $ convertString dsType
        addr <- C.alloca t
        val <- C.toCodegen dsExpr
        C.store addr val
        C.addVariable (convertString dsVar) t addr

instance C.ToCodegen OT.ExprStmt () where
    toCodegen OT.ExprStmt{..} = do
        v <- C.toCodegen esExpr
        maybe (return ()) (assign v) esVar
      where
        assign val varExpr = do
            addr <- C.toCodegenPtr varExpr
            C.store addr val

instance C.ToCodegen OT.FlowStmt () where
    toCodegen (OT.FSReturn rs) = C.toCodegen rs

instance C.ToCodegen OT.ReturnStmt () where
    toCodegen (OT.ReturnStmt me) =
        () <$ maybe (C.ret Nothing) (C.ret . Just . snd <=< C.toCodegen) me

instance C.ToCodegen OT.Expr C.TypedOperand where
    toCodegen (OT.EUnary OT.UnaryAddr a) = C.toCodegenPtr a
    toCodegen (OT.EUnary uOp a) = C.toCodegen a >>= convertUnOp uOp
      where
        convertUnOp OT.UnaryPlus = pure
        convertUnOp OT.UnaryMinus = C.neg
        convertUnOp OT.UnaryNot = C.not
        convertUnOp OT.UnaryDeref = C.load
        convertUnOp OT.UnaryAddr = undefined
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

methodCall :: C.TypedOperand -> OT.Identifier -> [OT.Expr] -> C.Codegen C.TypedOperand
methodCall varPtr@(OT.TPointer (OT.TClass className _),_) methodName exprs = do
    let mangledName = C.mangleClassMethodName className methodName
    fPtr <- C.lookupName $ convertString mangledName
    args <- (varPtr :) <$> mapM C.toCodegen exprs
    C.call fPtr args
methodCall (t,_) _ _ =
    C.throwCodegenError $
    formatSingle' "attempt to call method of something strange (type is {})" t

instance C.ToCodegen OT.AtomExpr C.TypedOperand where
    toCodegen (OT.AEAtom a) = C.toCodegen a
    toCodegen (OT.AECall (OT.AEAccess expr methodName) exprs) = do
        varPtr <- C.toCodegenPtr expr
        methodCall varPtr methodName exprs
    toCodegen (OT.AECall f exprs) =
        join $ C.call <$> C.toCodegen f <*> mapM C.toCodegen exprs
    toCodegen (OT.AEAccess expr fieldName) =
        flip C.lookupMember (convertString fieldName) =<< C.toCodegenPtr expr

instance C.ToCodegen OT.Atom C.TypedOperand where
    toCodegen (OT.AExpr e) = C.toCodegen e
    toCodegen (OT.AIdentifier v) = C.lookupName $ convertString v
    toCodegen (OT.ANumber n) = (OT.TInt64,) . AST.ConstantOperand <$> C.toConstant n
    toCodegen (OT.ABool b) = (OT.TBool,) . AST.ConstantOperand <$> C.toConstant b

instance C.ToCodegen OT.CompoundStmt () where
    toCodegen (OT.CSIf s) = C.toCodegen s
    toCodegen (OT.CSWhile s) = C.toCodegen s
    toCodegen (OT.CSFunc _) =
        C.throwCodegenError "nested functions are not supported"
    toCodegen (OT.CSClass _) =
        C.throwCodegenError "class definition is allowed only as top-level"

instance C.ToCodegen OT.IfStmt () where
    toCodegen (OT.IfStmt expr trueBody falseBody) = do
        trueBlock <- C.addBlock "if.then"
        contBlock <- C.addBlock "if.cont"
        falseBlock <-
            maybe (pure contBlock) (const $ C.addBlock "if.else") falseBody
        condOperand <- C.toCodegen expr
        () <$ C.cbr condOperand trueBlock falseBlock
        tTerminated <- generateCaseBody contBlock trueBlock trueBody
        fTerminated <- maybe (pure False) (generateCaseBody contBlock falseBlock) falseBody
        finalize (tTerminated && fTerminated) contBlock
      where
        generateCaseBody contBlock blockName blockBody = do
            C.setBlock blockName
            () <$ C.toCodegen blockBody
            isTerminated <- C.isTerminated
            if isTerminated
                then return True
                else C.br contBlock >> return False
        finalize True = C.removeBlock
        finalize False = C.setBlock

instance C.ToCodegen OT.WhileStmt () where
    toCodegen (OT.WhileStmt expr body) = do
        condBlock <- C.addBlock "while.cond"
        bodyBlock <- C.addBlock "while.body"
        contBlock <- C.addBlock "while.cont"
        () <$ C.br condBlock
        C.setBlock condBlock
        v <- C.toCodegen expr
        () <$ C.cbr v bodyBlock contBlock
        C.setBlock bodyBlock
        C.toCodegen body
        isTerminated <- C.isTerminated
        unless isTerminated $ () <$ C.br condBlock
        C.setBlock contBlock

instance C.ToCodegen OT.Suite () where
    toCodegen = mapM_ C.toCodegen . OT.getSuite

--------------------------------------------------------------------------------
-- C.ToCodegenPtr
--------------------------------------------------------------------------------

instance C.ToCodegenPtr OT.Expr where
    toCodegenPtr (OT.EAtom a) = C.toCodegenPtr a
    toCodegenPtr (OT.EUnary OT.UnaryDeref a) = C.toCodegen a
    toCodegenPtr _ =
        C.throwCodegenError "can't create lvalue ptr from result of operator"

instance C.ToCodegenPtr OT.AtomExpr where
    toCodegenPtr (OT.AEAtom a) = C.toCodegenPtr a
    toCodegenPtr (OT.AECall _ _) =
        C.throwCodegenError
            "can't create lvalue ptr from result of function call"
    toCodegenPtr (OT.AEAccess expr memberName) = do
        exprPtr <- C.toCodegenPtr expr
        C.getMemberPtr exprPtr $ convertString memberName

instance C.ToCodegenPtr OT.Atom where
    toCodegenPtr (OT.AExpr e) = C.toCodegenPtr e
    toCodegenPtr (OT.AIdentifier i) = C.getPtr $ convertString i
    toCodegenPtr _ = C.throwCodegenError "can't create lvalue ptr from constant"

--------------------------------------------------------------------------------
-- C.ToConstant
--------------------------------------------------------------------------------

instance C.ToConstant OT.Expr where
    toConstant (OT.EAtom a) = C.toConstant a
    toConstant _ =
        C.throwCodegenError
            "operator used on expressions can't be used as constant (it may be improved in later versions)"

instance C.ToConstant OT.AtomExpr where
    toConstant (OT.AEAtom a) = C.toConstant a
    toConstant (OT.AECall (OT.AEAtom (OT.AIdentifier f)) _) =
        C.complexConstant (convertString f) []
    toConstant (OT.AECall _ _) =
        C.throwCodegenError "function call can't be used as constant"
    toConstant (OT.AEAccess _ _) =
        C.throwCodegenError "field access can't be used as constant"

instance C.ToConstant OT.Atom where
    toConstant (OT.AExpr e) = C.toConstant e
    toConstant (OT.AIdentifier name) =
        C.throwCodegenError $
        formatSingle' "identifier ({}) can't be used as constant" name
    toConstant (OT.ANumber n) = C.toConstant n
    toConstant (OT.ABool b) = C.toConstant b
