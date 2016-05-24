{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE TemplateHaskell        #-}

-- | Module-level code generation.

module Orchid.Codegen.Module
       ( ModuleState
       , LLVM
       , ToLLVM
       , toLLVM
       , execLLVM
       , mkModuleState
       , getActiveClass
       , addFuncDef
       , addGlobalVariable
       , makeVarPrivate
       , makeFuncPrivate
       , startClassDef
       , finishClassDef
       ) where

import           Control.Lens              (at, makeLenses, makeLensesFor, set,
                                            use, view, (%=), (.=), (<>=), (^.),
                                            _Just)
import           Control.Monad             (when)
import           Control.Monad.Except      (ExceptT, MonadError (throwError),
                                            runExceptT)
import           Control.Monad.State       (MonadState, State, runState)
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import           Data.String.Conversions   (convertString)
import           Data.Text                 (Text)
import qualified LLVM.General.AST          as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Global   as G

import           Serokell.Util             (format')

import           Orchid.Codegen.Body       (ToBody (toBody), createBlocks,
                                            execBodyGen, lowLevelCast, ret)
import           Orchid.Codegen.Common     (ClassesMap,
                                            FunctionData (FunctionData),
                                            FunctionsMap,
                                            HasClasses (classesLens),
                                            VariableData (..), VariablesMap,
                                            cdVariables, classAndParents,
                                            cvInitializer, cvPrivate, cvType,
                                            fdArgTypes, fdRetType,
                                            mangleClassMethodName, mkClassData,
                                            mkClassVariable, orchidTypeToLLVM,
                                            throwCodegenError)
import           Orchid.Codegen.Constant   (ToConstant (toConstant))
import           Orchid.Codegen.Type       (Type (..))
import           Orchid.Error              (CodegenException)

-------------------------------------------------------------------------------
-- Module Level
-------------------------------------------------------------------------------

$(makeLensesFor [("moduleDefinitions", "mDefinitions")] ''AST.Module)

-- | ModuleState is a state of the whole module.
data ModuleState = ModuleState
    { _msFunctions        :: FunctionsMap
    , _msPrivateFunctions :: S.Set Text
    , _msClasses          :: ClassesMap
    , _msVariables        :: VariablesMap
    , _msClass            :: Maybe Text
    , _msModule           :: AST.Module
    } deriving (Show)

$(makeLenses ''ModuleState)

instance HasClasses ModuleState where
    classesLens = msClasses

-- | LLVM is a Monad intended to be used for module code generation.
newtype LLVM a = LLVM
    { getLLVM :: ExceptT CodegenException (State ModuleState) a
    } deriving (Functor,Applicative,Monad,MonadState ModuleState,MonadError CodegenException)

class ToLLVM a  where
    toLLVM :: a -> LLVM ()

-- | Execute LLVM Monad to produce a generated module.
execLLVM :: ModuleState -> LLVM a -> Either CodegenException AST.Module
execLLVM m = f . flip runState m . runExceptT . getLLVM
  where
    f (Left e,_) = Left e
    f (_,s') = Right $ s' ^. msModule

-- | Make initial module state with given name and some predefined
-- functions and variables.
mkModuleState
    :: Text
    -> AST.Module
    -> FunctionsMap
    -> ClassesMap
    -> VariablesMap
    -> ModuleState
mkModuleState moduleName preludeModule preludeFunctions preludeClasses preludeVariables =
    ModuleState
    { _msFunctions = preludeFunctions
    , _msPrivateFunctions = S.empty
    , _msClasses = preludeClasses
    , _msVariables = preludeVariables
    , _msClass = Nothing
    , _msModule = preludeModule
      { AST.moduleName = convertString moduleName
      }
    }

-- | Get name of active class (if any).
getActiveClass :: LLVM (Maybe Text)
getActiveClass = use msClass

addDefn :: AST.Definition -> LLVM ()
addDefn d = msModule . mDefinitions <>= [d]

-- | Add function with given return type, name, arguments and suite to
-- module. This function sets active class for given codegen.
addFuncDef
    :: (ToBody a r)
    => Type -> Text -> [(Type, AST.Name)] -> a -> LLVM ()
addFuncDef retType funcName args suite = do
    funcs <- use msFunctions
    privFuncs <- use msPrivateFunctions
    classes <- use msClasses
    vars <- use msVariables
    activeClass <- use msClass
    case bodyEither funcs privFuncs classes vars activeClass of
        Left e -> throwError e
        Right body -> do
            addDefn $ AST.GlobalDefinition $
                G.functionDefaults
                { G.name = funcName'
                , G.parameters = parameters
                , G.returnType = orchidTypeToLLVM retType
                , G.basicBlocks = body
                }
            msFunctions . at funcName .= Just funcData
  where
    funcName' = convertString funcName
    funcData =
        FunctionData
        { fdRetType = retType
        , fdArgTypes = map fst args
        }
    bodyEither funcs privFuncs classes vars activeClass =
        execBodyGen funcs privFuncs classes vars activeClass (toBody suite) >>=
        createBlocks
    parameters =
        ([G.Parameter (orchidTypeToLLVM t) n [] | (t,n) <- args], False)

-- | Outside class this function adds global variable with given type
-- and name to module. Inside class definition it adds given variable
-- as part of class.
addGlobalVariable
    :: (ToConstant a)
    => Type -> Text -> a -> LLVM ()
addGlobalVariable varType varName varExpr = do
    value <- toConstant varExpr
    addToGlobal value
  where
    varName' = convertString varName
    varType' = orchidTypeToLLVM varType
    varData =
        VariableData
        { vdType = varType
        , vdPtrOperand = (AST.ConstantOperand $ C.GlobalReference varType' $
                          varName')
        }
    addToGlobal value = do
        addDefn $ AST.GlobalDefinition $
            G.globalVariableDefaults
            { G.name = varName'
            , G.type' = varType'
            , G.initializer = Just value
            }
        msVariables . at varName .= Just varData

-- | Make variable in active class private.
makeVarPrivate :: Text -> LLVM ()
makeVarPrivate varName =
    maybe (throwCodegenError outsideClassMsg) impl =<< use msClass
  where
    outsideClassMsg =
        "internal error: attempt to make variable private outside class"
    impl clsName =
        msClasses . at clsName . _Just . cdVariables . at varName %=
        fmap (set cvPrivate True)

-- | Make function in active class private.
makeFuncPrivate :: Text -> LLVM ()
makeFuncPrivate funcName =
    maybe (throwCodegenError outsideClassMsg) impl =<< use msClass
  where
    outsideClassMsg =
        "internal error: attempt to make function private outside class"
    impl clsName =
        msPrivateFunctions %= S.insert (mangleClassMethodName clsName funcName)

startClassDef :: Text
              -> Maybe Text
              -> [(Text, (Type, C.Constant))]
              -> [(Text, Type)]
              -> LLVM ()
startClassDef className parent members virtualMethods = do
    cls <- use msClass
    maybe
        addNewCls
        (const $
         throwCodegenError "class definition can't appear inside another class")
        cls
  where
    newClass parentVars = mkClassData (M.unions parentVars) parent
    convertVirtualMethod (methodName,methodType) =
        let t = TPointer methodType
            mangledName = mangleClassMethodName className methodName
        in ( methodName
           , (t, C.GlobalReference (orchidTypeToLLVM t) (convertString mangledName)))
    members' = map convertVirtualMethod virtualMethods ++ members
    addNewCls = do
        msClasses %= M.insert className (newClass [])
        parents <- tail <$> classAndParents (Just className)
        parentVars <-
            mapM
                (\n ->
                      use $ msClasses . at n . _Just . cdVariables)
                parents
        msClasses . at className .= Just (newClass parentVars)
        mapM_ addClassVar members'
        use (msClasses . at className . _Just . cdVariables) >>= addConstructor
        msClass .= Just className
    addConstructor variables = do
        let memberTypes = map (view cvType) $ M.elems variables
        addFuncDef (TClass className memberTypes) className [] $
            constructorBody variables
    constructorBody variables = do
        let operand =
                AST.ConstantOperand . C.Struct Nothing False .
                map (view cvInitializer) .
                M.elems $
                variables
        let expectedType =
                TClass className . map (view cvType) . M.elems $
                variables
        ret . Just . snd =<< lowLevelCast operand expectedType
    isFunctionPointerType (TPointer (TFunction _ _)) = True
    isFunctionPointerType _ = False
    addClassVar (varName,(varType,varInitializer)) = do
        alreadyAdded <-
            M.member varName <$>
            use (msClasses . at className . _Just . cdVariables)
        when (alreadyAdded && Prelude.not (isFunctionPointerType varType)) $
            throwCodegenError $
            format'
                "variable {} is defined in class {} more than once"
                (varName, className)
        msClasses . at className . _Just . cdVariables . at varName .=
            Just (clsVarData varType varInitializer)
    clsVarData t value = mkClassVariable t value False

finishClassDef :: LLVM ()
finishClassDef = maybe reportError (const $ msClass .= Nothing) =<< use msClass
  where
    reportError =
        throwCodegenError
            "internal error: attempt to finish definition of class outside of class"
