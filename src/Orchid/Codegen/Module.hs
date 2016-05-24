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
       , addFuncDef
       , addGlobalVariable
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
import           Data.List                 (find)
import qualified Data.Map                  as M
import           Data.Maybe                (isJust)
import qualified Data.Set                  as S
import           Data.String.Conversions   (convertString)
import           Data.Text                 (Text)
import qualified LLVM.General.AST          as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Global   as G

import           Serokell.Util             (format')

import           Orchid.Codegen.Body       (ToBody (toBody), createBlocks,
                                            execBodyGen)
import qualified Orchid.Codegen.Body       as B
import           Orchid.Codegen.Common     (ClassVariable, ClassesMap,
                                            FunctionData (FunctionData),
                                            FunctionsMap,
                                            HasClasses (classesLens),
                                            VariableData (..), VariablesMap,
                                            cdVariables, classAndParents,
                                            cvInitializer, cvName, cvType,
                                            fdArgTypes, fdRetType,
                                            lookupClassType,
                                            mangleClassMethodName, mkClassData,
                                            orchidTypeToLLVM, thisPtrName,
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

addDefn :: AST.Definition -> LLVM ()
addDefn d = msModule . mDefinitions <>= [d]

-- | Add function with given return type, name, arguments and suite to
-- module. This function sets active class for given codegen.
addFuncDef'
    :: (B.ToBody a r)
    => Type -> Text -> [(Type, Text)] -> a -> LLVM ()
addFuncDef' retType funcName args suite = do
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
        ( [G.Parameter (orchidTypeToLLVM t) (convertString n) [] | (t,n) <- args]
        , False)

addFuncDef :: B.ToBody a r => Type -> Text -> [(Type, Text)] -> a -> LLVM ()
addFuncDef retType funcName args suite = do
    maybe globalCase classCase =<< use msClass
  where
    globalCase = addFuncDef' retType funcName args bodyGlobal
    bodyGlobal = do
        mapM_ storeArgument args
        B.toBody suite
    storeArgument (argType,argName) = do
        addr <- B.alloca argType
        B.store addr $
            ( argType
            , AST.LocalReference
                  (orchidTypeToLLVM argType)
                  (convertString argName))
        B.addVariable argName argType addr
    classCase className = do
        classType <- lookupClassType className
        let thisArg = (TPointer classType, thisPtrName)
            mangledFuncName = mangleClassMethodName className funcName
        addFuncDef' retType mangledFuncName (thisArg : args) $
            bodyClass classType
    bodyClass classType = do
        let thisType = TPointer classType
        B.addVariable thisPtrName classType $
            ( thisType
            , AST.LocalReference (orchidTypeToLLVM thisType) thisPtrName)
        mapM_ storeArgument args
        B.toBody suite

-- | This function adds global variable with given type
-- and name to module.
addGlobalVariable
    :: (ToConstant a)
    => Type -> Text -> a -> LLVM ()
addGlobalVariable varType varName varExpr = addToGlobal =<< toConstant varExpr
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
              -> [ClassVariable]
              -> LLVM ()
startClassDef className parent members = do
    cls <- use msClass
    when (isJust cls) $
        throwCodegenError "class definition can't appear inside another class"
    msClasses %= M.insert className (mkClassData [] parent)
    parents <- tail <$> classAndParents (Just className)
    parentVars <-
        mapM
            (\n ->
                  use $ msClasses . at n . _Just . cdVariables)
            parents
    msClasses . at className %= fmap (set cdVariables (concat parentVars))
    mapM_ addClassVar members
    use variablesLens >>= addConstructor
    msClass .= Just className
  where
    -- convertVirtualMethod (methodName,methodType) =
    --     let t = TPointer methodType
    --         mangledName = mangleClassMethodName className methodName
    --     in ( methodName
    --        , (t, C.GlobalReference (orchidTypeToLLVM t) (convertString mangledName)))
    -- members' = map convertVirtualMethod virtualMethods ++ members
    variablesLens = msClasses . at className . _Just . cdVariables
    addConstructor variables = do
        let memberTypes = map (view cvType) variables
        addFuncDef' (TClass className memberTypes) className [] $
            constructorBody variables
    constructorBody variables = do
        let operand =
                AST.ConstantOperand . C.Struct Nothing False .
                map (view cvInitializer) $
                variables
            expectedType = TClass className . map (view cvType) $ variables
        -- B.ret . Just . snd =<< B.lowLevelCast operand expectedType
        B.ret .
            Just .
            snd $
            (expectedType, operand)
    -- isFunctionPointerType (TPointer (TFunction _ _)) = True
    -- isFunctionPointerType _ = False
    addClassVar classVar = do
        alreadyAdded <-
            isJust . find ((== classVar ^. cvName) . view cvName) <$>
            use variablesLens
        when alreadyAdded $ throwCodegenError $
            format'
                "variable {} is defined in class {} more than once"
                (classVar ^. cvName, className)
        msClasses . at className . _Just . cdVariables <>= [classVar]

finishClassDef :: LLVM ()
finishClassDef = maybe reportError (const $ msClass .= Nothing) =<< use msClass
  where
    reportError =
        throwCodegenError
            "internal error: attempt to finish definition of class outside of class"
