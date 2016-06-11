{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE TemplateHaskell        #-}

-- | Module-level code generation.

module Orchid.Codegen.Module
       ( ModuleState
       , msOptimizeTailRecursion
       , LLVM
       , ToLLVM
       , toLLVM
       , execLLVM
       , mkModuleState
       , declareFunction
       , addFuncDef
       , addGlobalVariable
       , declareClass
       , startClassDef
       , makeFuncPrivate
       , finishClassDef
       ) where

import           Control.Lens               (at, ix, makeLenses, makeLensesFor,
                                             set, use, view, (%=), (&), (.=),
                                             (.~), (<>=), (^.), _Just)
import           Control.Monad              (when)
import           Control.Monad.Except       (ExceptT, MonadError (throwError),
                                             runExceptT)
import           Control.Monad.State        (MonadState, State, runState)
import           Data.Foldable              (foldlM)
import           Data.List                  (find, findIndex)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromJust, isJust)
import qualified Data.Set                   as S
import           Data.String.Conversions    (convertString)
import           Data.Text                  (Text)
import qualified LLVM.General.AST           as AST
import           LLVM.General.AST.AddrSpace (AddrSpace (AddrSpace))
import qualified LLVM.General.AST.Constant  as C
import qualified LLVM.General.AST.Global    as G

import           Serokell.Util              (format')

import           Orchid.Codegen.Body        (ToBody (toBody), createBlocks,
                                             execBodyGen)
import qualified Orchid.Codegen.Body        as B
import           Orchid.Codegen.Common      (ClassVariable, ClassesMap,
                                             FunctionData (FunctionData),
                                             FunctionsMap,
                                             HasClasses (classesLens),
                                             VariableData (..), VariablesMap,
                                             cdAllVMethods, cdOurVMethods,
                                             cdVariables, classAndParents,
                                             compareTypesSmart, cvInitializer,
                                             cvName, cvType, fdArgTypes,
                                             fdRetType, lookupClassType,
                                             mangleClassMethodName, mkClassData,
                                             orchidTypeToLLVM, thisPtrName,
                                             throwCodegenError, vTableName,
                                             vTableType, vTableTypeName)
import           Orchid.Codegen.Constant    (ToConstant (toConstant))
import           Orchid.Codegen.Type        (Type (..))
import           Orchid.Error               (CodegenException)

-------------------------------------------------------------------------------
-- Module Level
-------------------------------------------------------------------------------

$(makeLensesFor [("moduleDefinitions", "mDefinitions")] ''AST.Module)

-- | ModuleState is a state of the whole module.
data ModuleState = ModuleState
    { _msFunctions             :: !FunctionsMap  -- ^ Global functions
                                                 -- defined within
                                                 -- module
    , _msPrivateFunctions      :: !(S.Set Text)  -- ^ Which functions are private
    , _msClasses               :: !ClassesMap    -- ^ Classes defined
                                                 -- within module
    , _msVariables             :: !VariablesMap  -- ^ Global variables
                                                 -- defined within
                                                 -- module
    , _msClass                 :: !(Maybe Text)  -- ^ Active class
    , _msModule                :: !AST.Module    -- ^ LLVM Module
    , _msOptimizeTailRecursion :: !Bool          -- ^ Whether tail
                                                 -- recursion should
                                                 -- be optimized
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
    -> Bool
    -> AST.Module
    -> FunctionsMap
    -> ClassesMap
    -> VariablesMap
    -> ModuleState
mkModuleState moduleName optimizeTailRecursion preludeModule preludeFunctions preludeClasses preludeVariables =
    ModuleState
    { _msFunctions = preludeFunctions
    , _msPrivateFunctions = S.empty
    , _msClasses = preludeClasses
    , _msVariables = preludeVariables
    , _msClass = Nothing
    , _msModule = preludeModule
      { AST.moduleName = convertString moduleName
      }
    , _msOptimizeTailRecursion = optimizeTailRecursion
    }

declareFunction :: Type -> Text -> [(Type, Text)] -> LLVM ()
declareFunction retType funcName args =
    msFunctions . at funcName .= Just funcData
  where
    funcData =
        FunctionData
        { fdRetType = retType
        , fdArgTypes = map fst args
        }

addDefn :: AST.Definition -> LLVM ()
addDefn d = msModule . mDefinitions <>= [d]

-- | Add function with given return type, name, arguments and suite to
-- module taking active class into account.
addFuncDef :: B.ToBody a r => Type -> Text -> [(Type, Text)] -> a -> LLVM ()
addFuncDef retType funcName args suite = do
    maybe globalCase classCase =<< use msClass
  where
    globalCase = addFuncDefDo retType funcName args bodyGlobal
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
        addFuncDefDo retType mangledFuncName (thisArg : args) $
            bodyClass classType
    bodyClass classType = do
        let thisType = TPointer classType
        B.addVariable thisPtrName classType $
            ( thisType
            , AST.LocalReference (orchidTypeToLLVM thisType) thisPtrName)
        mapM_ storeArgument args
        B.toBody suite

addFuncDefDo
    :: (B.ToBody a r)
    => Type -> Text -> [(Type, Text)] -> a -> LLVM ()
addFuncDefDo retType funcName args suite = do
    msFunctions . at funcName .= Just funcData
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

declareClass :: Text
             -> Maybe Text
             -> [ClassVariable]
             -> [(Text, Type)]
             -> LLVM ()
declareClass className parent members virtualMethods = do
    insertDummyClassData className parent
    parents <- tail <$> classAndParents (Just className)
    defineVTable className parents virtualMethods
    parentVars <-
        mapM
            (\n ->
                  use $ msClasses . at n . _Just . cdVariables)
            parents
    msClasses . at className %= fmap (set cdVariables (concat parentVars))
    existingMembers <- use variablesLens
    mapM_ addClassVarSafe members
    let allMembers = existingMembers ++ members
    addTypeDefinition allMembers
    addConstructor allMembers
  where
    variablesLens = msClasses . at className . _Just . cdVariables
    vTableType' =
        AST.NamedTypeReference $ convertString $ vTableTypeName className
    vTablePtrType' = AST.PointerType vTableType' $ AddrSpace 0
    addTypeDefinition =
        addDefn . AST.TypeDefinition (convertString className) . Just .
        AST.StructureType False .
        (vTablePtrType' :) .
        map (orchidTypeToLLVM . view cvType)
    addConstructor variables =
        addFuncDefDo
            (TClass className $ map (view cvType) variables)
            className
            [] $
        constructorBody variables
    constructorBody variables = do
        let operand =
                AST.ConstantOperand .
                C.Struct (Just . convertString $ className) False .
                (C.GlobalReference
                     vTableType'
                     (convertString $ vTableName className) :) .
                map (view cvInitializer) $
                variables
        B.ret . Just $ operand
    addClassVarSafe classVar = do
        alreadyAdded <-
            isJust . find ((== classVar ^. cvName) . view cvName) <$>
            use variablesLens
        when alreadyAdded $ throwCodegenError $
            format'
                "variable {} is defined in class {} more than once"
                (classVar ^. cvName, className)
        msClasses . at className . _Just . cdVariables <>= [classVar]

checkNestedClass :: LLVM ()
checkNestedClass = do
    cls <- use msClass
    when (isJust cls) $
        throwCodegenError "class definition can't appear inside another class"

-- A little bit hacky solution to make classAndParents function work.
insertDummyClassData :: Text -> Maybe Text -> LLVM()
insertDummyClassData className parent =
    msClasses %= M.insert className (mkClassData [] parent [] [])

data ExtendedVirtualMethod = ExtendedVirtualMethod
    { evmClass :: !Text
    , evmName  :: !Text
    , evmType  :: !Type
    } deriving (Show)

defineVTable :: Text -> [Text] -> [(Text, Type)] -> LLVM ()
defineVTable className parents virtualMethods = do
    parentVirtualMethods <-
        mapM
            (\n ->
                  map (uncurry (ExtendedVirtualMethod n)) <$>
                  use (msClasses . at n . _Just . cdOurVMethods))
            parents
    let ourVirtualMethods =
            map
                ((\(mName,mType) ->
                       ExtendedVirtualMethod className mName $ TPointer $
                       addThisPtrType className mType))
                virtualMethods
    jointVirtualMethods <-
        joinVirtualMethods parentVirtualMethods ourVirtualMethods
    let setVMethods lens methods =
            msClasses . at className %=
            fmap
                (set lens $
                 map
                     (\ExtendedVirtualMethod{..} ->
                           (evmName, evmType))
                     methods)
    setVMethods cdOurVMethods ourVirtualMethods
    setVMethods cdAllVMethods jointVirtualMethods
    cd <- fromJust <$> use (msClasses . at className)
    addVTableTypeDefinition jointVirtualMethods
    addVTable cd jointVirtualMethods
  where
    realVTableType jointVirtualMethods =
        AST.StructureType False . map (orchidTypeToLLVM . evmType) $
        jointVirtualMethods
    addVTableTypeDefinition =
        addDefn . AST.TypeDefinition (convertString $ vTableTypeName className) .
        Just .
        realVTableType
    addVTable cd jointVirtualMethods =
        addDefn . AST.GlobalDefinition $
        G.globalVariableDefaults
        { G.name = convertString $ vTableName className
        , G.type' = orchidTypeToLLVM $ vTableType className cd
        , G.initializer = Just .
          C.Struct (Just . convertString . vTableTypeName $ className) False .
          map virtualMethodConstant $
          jointVirtualMethods
        }
    virtualMethodConstant ExtendedVirtualMethod{..} =
        C.GlobalReference
            (orchidTypeToLLVM evmType)
            (convertString $ mangleClassMethodName evmClass evmName)
    addThisPtrType methodClass (TFunction retType argTypes) =
        TFunction retType $ TPointer (TClass methodClass []) : argTypes
    addThisPtrType _ _ = error "addThisPtrType: not TFunction"

joinVirtualMethods
    :: [[ExtendedVirtualMethod]]
    -> [ExtendedVirtualMethod]
    -> LLVM [ExtendedVirtualMethod]
joinVirtualMethods parentMethods ourMethods =
    foldlM step [] (concat (reverse parentMethods) ++ ourMethods)
  where
    step
        :: [ExtendedVirtualMethod]
        -> ExtendedVirtualMethod
        -> LLVM [ExtendedVirtualMethod]
    step l evm =
        case findIndex ((evmName evm ==) . evmName) l of
            Nothing -> pure $ l ++ [evm]
            Just i -> do
                let tExpected = evmType (l !! i)
                    tFound = evmType evm
                typeMismatch <- not <$> compareTypesSmart tExpected tFound
                when typeMismatch $
                    throwCodegenError $
                    format'
                        "Virtual method type mismatch (found {}, expected {})"
                        (tFound, tExpected)
                return $ (l & ix i .~ evm)

startClassDef :: Text -> LLVM ()
startClassDef className = do
    checkNestedClass
    msClass .= Just className

-- | Make function in active class private.
makeFuncPrivate :: Text -> LLVM ()
makeFuncPrivate funcName =
    maybe (throwCodegenError outsideClassMsg) impl =<< use msClass
  where
    outsideClassMsg =
        "internal error: attempt to make function private outside class"
    impl clsName =
        msPrivateFunctions %= S.insert (mangleClassMethodName clsName funcName)

finishClassDef :: LLVM ()
finishClassDef = maybe reportError (const $ msClass .= Nothing) =<< use msClass
  where
    reportError =
        throwCodegenError
            "internal error: attempt to finish definition of class outside of class"
