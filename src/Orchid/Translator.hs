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
import           Data.Text               (Text)
import qualified LLVM.General            as G
import qualified LLVM.General.AST        as AST
import           LLVM.General.Context    (withContext)
import           Serokell.Util           (formatSingle')

import qualified Orchid.Codegen          as C
import           Orchid.Error            (CodegenException,
                                          FatalError (FatalError),
                                          LowLevelException (LowLevelException))
import qualified Orchid.Types            as OT

type ModuleName = Text

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
            [ ("stdReadInt", C.FunctionData C.TInt64 [])
            , ("stdWriteInt", C.FunctionData C.TVoid [C.TInt64])
            , ("stdExit", C.FunctionData C.TVoid [C.TInt64])]
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
    translate
        (convertString fp)
        input
        (handleError . G.writeLLVMAssemblyToFile (G.File fp))

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

convertTypedArg :: OT.TypedArgument -> C.LLVM (C.Type, Text)
convertTypedArg OT.TypedArgument{..} =
    (, taName) . pointerIfNeeded <$> C.lookupType taType
  where
    pointerIfNeeded
      | taPointer = C.TPointer
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
    toLLVM OT.DeclStmt{..} = do
        t <- C.lookupType dsType
        C.addGlobalVariable t dsVar dsExpr

instance C.ToLLVM OT.CompoundStmt where
    toLLVM (OT.CSIf _) =
        C.throwCodegenError "'if' is not a valid top-level statement"
    toLLVM (OT.CSWhile _) =
        C.throwCodegenError "'while' is not a valid top-level statement"
    toLLVM (OT.CSFunc s) = C.toLLVM s
    toLLVM (OT.CSClass s) = C.toLLVM s

instance C.ToLLVM OT.FuncDef where
    toLLVM OT.FuncDef{..} = do
        ret <- maybe (return C.TVoid) C.lookupType funcRet
        args <- mapM convertTypedArg funcArgs
        C.addFuncDef ret funcName args funcBody

instance C.ToLLVM OT.ClassDef where
    toLLVM OT.ClassDef{..} = do
        join $ C.startClassDef clsName clsParent <$> members -- <*> virtualMethods
        C.toLLVM clsBody
        C.finishClassDef
      where
        members =
            catMaybes <$>
            (mapM classStmtToMember . OT.getClassSuite $
             clsBody)
        classStmtToMember (OT.ClassStmt _ (Left _)) = pure Nothing
        classStmtToMember (OT.ClassStmt access (Right OT.DeclStmt{..})) =
            Just <$> (C.mkClassVariable dsVar <$> C.lookupType dsType <*>
            C.toConstant dsExpr <*>
            pure (access == OT.AMPrivate))
        -- virtualMethods =
        --     catMaybes <$>
        --     (mapM payloadToVirtualFunction .
        --      map OT.csPayload . OT.getClassSuite $
        --      clsBody)
        -- payloadToVirtualFunction (Left (True,OT.FuncDef{..})) =
        --     Just . (funcName, ) <$>
        --     do args <- mapM (fmap fst . convertTypedArg) funcArgs
        --        ret <- maybe (pure C.TVoid) C.lookupType funcRet
        --        return $ C.TFunction ret args
        -- payloadToVirtualFunction _ = pure Nothing

instance C.ToLLVM OT.ClassSuite where
    toLLVM = mapM_ C.toLLVM . OT.getClassSuite

instance C.ToLLVM OT.ClassStmt where
    toLLVM OT.ClassStmt{csAccess = access,csPayload = Left (_,f)} = do
        C.toLLVM f
        when (access == OT.AMPrivate) $ C.makeFuncPrivate $ OT.funcName f
    toLLVM OT.ClassStmt{csAccess = _,csPayload = Right _} = return ()

--------------------------------------------------------------------------------
-- C.ToBody
--------------------------------------------------------------------------------

instance C.ToBody OT.Stmt () where
    toBody (OT.SSimple s) = C.toBody s
    toBody (OT.SCompound s) = C.toBody s

instance C.ToBody OT.SimpleStmt () where
    toBody (OT.SimpleStmt smallStmts) = mapM_ C.toBody smallStmts

instance C.ToBody OT.SmallStmt () where
    toBody (OT.SSDecl d) = C.toBody d
    toBody (OT.SSExpr e) = C.toBody e
    toBody OT.SSPass = return ()
    toBody (OT.SSFlow fs) = C.toBody fs

instance C.ToBody OT.DeclStmt () where
    toBody OT.DeclStmt{..} = do
        t <- C.lookupType dsType
        addr <- C.alloca t
        val <- C.toBody dsExpr
        C.store addr val
        C.addVariable dsVar t addr

instance C.ToBody OT.ExprStmt () where
    toBody OT.ExprStmt{..} = do
        v <- C.toBody esExpr
        maybe (return ()) (assign v) esVar
      where
        assign val varExpr = do
            addr <- C.toBodyPtr varExpr
            C.store addr val

instance C.ToBody OT.FlowStmt () where
    toBody (OT.FSReturn rs) = C.toBody rs

instance C.ToBody OT.ReturnStmt () where
    toBody (OT.ReturnStmt me) =
        () <$ maybe (C.ret Nothing) (C.ret . Just . snd <=< C.toBody) me

instance C.ToBody OT.Expr C.TypedOperand where
    toBody (OT.EUnary OT.UnaryAddr a) = C.toBodyPtr a
    toBody (OT.EUnary uOp a) = C.toBody a >>= convertUnOp uOp
      where
        convertUnOp OT.UnaryPlus = pure
        convertUnOp OT.UnaryMinus = C.neg
        convertUnOp OT.UnaryNot = C.not
        convertUnOp OT.UnaryDeref = C.load
        convertUnOp OT.UnaryAddr = undefined
    toBody (OT.EBinary bOp a b) = do
        op1 <- C.toBody a
        op2 <- C.toBody b
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
                 do f <- C.toBody (OT.AIdentifier "stdPower")
                    C.call f [a', b']
    toBody (OT.EAtom a) = C.toBody a

instance C.ToBody OT.AtomExpr C.TypedOperand where
    toBody (OT.AEAtom a) = C.toBody a
    toBody (OT.AECall (OT.AEAccess expr methodName) exprs) = do
        varPtr <- C.toBodyPtr expr
        C.methodCall varPtr methodName =<< mapM C.toBody exprs
    toBody (OT.AECall f exprs) =
        join $ C.call <$> C.toBody f <*> mapM C.toBody exprs
    toBody (OT.AEAccess expr fieldName) =
        flip C.memberNameToValue fieldName =<< C.toBodyPtr expr

instance C.ToBody OT.Atom C.TypedOperand where
    toBody (OT.AExpr e) = C.toBody e
    toBody (OT.AIdentifier v) = C.nameToValue v
    toBody (OT.ANumber n) = (C.TInt64,) <$> C.toConstantOperand n
    toBody (OT.ABool b) = (C.TBool,) <$> C.toConstantOperand b

instance C.ToBody OT.CompoundStmt () where
    toBody (OT.CSIf s) = C.toBody s
    toBody (OT.CSWhile s) = C.toBody s
    toBody (OT.CSFunc _) =
        C.throwCodegenError "nested functions are not supported"
    toBody (OT.CSClass _) =
        C.throwCodegenError "class definition is allowed only as top-level"

instance C.ToBody OT.IfStmt () where
    toBody (OT.IfStmt expr trueBody falseBody) = do
        trueBlock <- C.addEmptyBlock "if.then"
        contBlock <- C.addEmptyBlock "if.cont"
        falseBlock <-
            maybe (pure contBlock) (const $ C.addEmptyBlock "if.else") falseBody
        condOperand <- C.toBody expr
        () <$ C.cbr condOperand trueBlock falseBlock
        tTerminated <- generateCaseBody contBlock trueBlock trueBody
        fTerminated <- maybe (pure False) (generateCaseBody contBlock falseBlock) falseBody
        finalize (tTerminated && fTerminated) contBlock
      where
        generateCaseBody contBlock blockName blockBody = do
            C.setBlock blockName
            () <$ C.toBody blockBody
            isTerminated <- C.isTerminated
            if isTerminated
                then return True
                else C.br contBlock >> return False
        finalize True = C.removeBlock
        finalize False = C.setBlock

instance C.ToBody OT.WhileStmt () where
    toBody (OT.WhileStmt expr body) = do
        condBlock <- C.addEmptyBlock "while.cond"
        bodyBlock <- C.addEmptyBlock "while.body"
        contBlock <- C.addEmptyBlock "while.cont"
        () <$ C.br condBlock
        C.setBlock condBlock
        v <- C.toBody expr
        () <$ C.cbr v bodyBlock contBlock
        C.setBlock bodyBlock
        C.toBody body
        isTerminated <- C.isTerminated
        unless isTerminated $ () <$ C.br condBlock
        C.setBlock contBlock

instance C.ToBody OT.Suite () where
    toBody = mapM_ C.toBody . OT.getSuite

--------------------------------------------------------------------------------
-- C.ToBodyPtr
--------------------------------------------------------------------------------

instance C.ToBodyPtr OT.Expr where
    toBodyPtr (OT.EAtom a) = C.toBodyPtr a
    toBodyPtr (OT.EUnary OT.UnaryDeref a) = C.toBody a
    toBodyPtr _ =
        C.throwCodegenError "can't create lvalue ptr from result of operator"

instance C.ToBodyPtr OT.AtomExpr where
    toBodyPtr (OT.AEAtom a) = C.toBodyPtr a
    toBodyPtr (OT.AECall _ _) =
        C.throwCodegenError
            "can't create lvalue ptr from result of function call"
    toBodyPtr (OT.AEAccess expr memberName) = do
        exprPtr <- C.toBodyPtr expr
        C.memberNameToPtr exprPtr memberName

instance C.ToBodyPtr OT.Atom where
    toBodyPtr (OT.AExpr e) = C.toBodyPtr e
    toBodyPtr (OT.AIdentifier i) = C.nameToPtr i
    toBodyPtr _ = C.throwCodegenError "can't create lvalue ptr from constant"

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
        C.complexConstant f []
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
