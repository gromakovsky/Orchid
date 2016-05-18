{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE TemplateHaskell        #-}

-- | Primitive functions to work with LLVM code generation.

module Orchid.Codegen
       (
         -- Helpers
         throwCodegenError
       , thisPtrName

         -- Useful types
       , FunctionData (..)
       , TypedOperand

         -- Codegen
       , Codegen
       , ToCodegen
       , toCodegen
       , createBlocks
       , execCodegen

         -- Symbol table
       , addVariable
       , lookupName
       , getPtr

         -- Block stack
       , addBlock
       , setBlock
       , removeBlock
       , getCurrentBlockName
       , getCurrentBlock
       , isTerminated

         -- Unary operators
       , neg
       , not

         -- Binary operators
       , or
       , and
       , xor
       , add
       , lessThan
       , greaterThan
       , equal
       , lessOrEqual
       , notEqual
       , greaterOrEqual
       , sub
       , mul
       , div
       , mod

         -- Constants
       , constInt64
       , constBool
       , ToConstant
       , toConstant
       , complexConstant

         -- Effects
       , call
       , alloca
       , store
       , load

         -- Control Flow
       , br
       , cbr
       -- , phi
       , ret

         -- Module level
       , ModuleState
       , LLVM
       , ToLLVM
       , toLLVM
       , execLLVM
       , mkModuleState
       , getActiveClass
       , addFuncDef
       , addGlobalVariable
       , startClassDef
       , finishClassDef

         -- Types
       , lookupType
       , classPointerType
       , orchidTypeToLLVM

         -- Reexports
       , AST.Name (..)
       ) where

import           Control.Applicative                ((<|>))
import           Control.Lens                       (Lens', at, makeLenses,
                                                     makeLensesFor, to, use,
                                                     view, (%=), (+=), (.=),
                                                     (.~), (<>=), (<>~), (^.),
                                                     _2, _Just)
import           Control.Monad                      (unless, when, (>=>))
import           Control.Monad.Except               (ExceptT,
                                                     MonadError (throwError),
                                                     runExceptT)
import           Control.Monad.State                (MonadState, State,
                                                     execState, runState)
import           Data.Bifunctor                     (first)
import           Data.Function                      (on, (&))
import           Data.Int                           (Int32, Int64)
import           Data.List                          (sortBy)
import qualified Data.Map                           as M
import           Data.Maybe                         (isJust)
import           Data.String                        (IsString (fromString))
import           Data.String.Conversions            (convertString)
import           Data.Text                          (Text)
import           Data.Text.Buildable                (Buildable (build))
import qualified LLVM.General.AST                   as AST
import           LLVM.General.AST.AddrSpace         (AddrSpace (AddrSpace))
import qualified LLVM.General.AST.Attribute         as A
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Constant          as C
import qualified LLVM.General.AST.Global            as G
import qualified LLVM.General.AST.IntegerPredicate  as IP
import qualified LLVM.General.AST.Type              as T (Type (..), i1, i64,
                                                          void)
import           Prelude                            hiding (and, div, mod, not,
                                                     or)
import qualified Prelude
import           Serokell.Util                      (format', formatSingle')

import           Orchid.Error                       (CodegenException (CodegenException))
import           Orchid.Types                       (Type (..))

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

throwCodegenError
    :: MonadError CodegenException m
    => Text -> m a
throwCodegenError = throwError . CodegenException

type Names = M.Map String Int

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
    case M.lookup nm ns of
        Nothing -> (nm, M.insert nm 1 ns)
        Just ix -> (nm ++ show ix, M.insert nm (ix + 1) ns)

instance IsString AST.Name where
    fromString = AST.Name . fromString

instance Buildable AST.Name where
    build (AST.Name a) = build a
    build (AST.UnName a) = build a

type Named = AST.Named

thisPtrName :: IsString s => s
thisPtrName = "this"

orchidTypeToLLVM :: Type -> T.Type
orchidTypeToLLVM TInt64 = T.i64
orchidTypeToLLVM TBool = T.i1
orchidTypeToLLVM TVoid = T.void
orchidTypeToLLVM (TPointer t) =
    T.PointerType (orchidTypeToLLVM t) (AddrSpace 0)
orchidTypeToLLVM (TFunction retType argTypes) =
    T.FunctionType
        (orchidTypeToLLVM retType)
        (map orchidTypeToLLVM argTypes)
        False
orchidTypeToLLVM (TClass _ types) =
    T.StructureType False $ map orchidTypeToLLVM types

-------------------------------------------------------------------------------
-- Codegen State
-------------------------------------------------------------------------------

-- | BlockState corresponds to LLVM's BasicBlock. It is constructed
-- step-by-step.
data BlockState = BlockState
    {
      -- ^ Block has an index which helps to sort blocks after they
      -- are constructed.
      _bsIdx          :: Word
    ,
      -- ^ Block contains ordered list of instructions which is
      -- initially empty and is populated step-by-step.
      _bsInstructions :: [Named AST.Instruction]
    ,
      -- ^ Block must contain a terminator. This field is optional,
      -- because block is constructed using iterative process.
      _bsTerm         :: Maybe (Named AST.Terminator)
    } deriving (Show)

$(makeLenses ''BlockState)

type BlocksMap = M.Map AST.Name BlockState

data VariableData = VariableData
    { vdType       :: Type
    , vdPtrOperand :: AST.Operand
    } deriving (Show)

variableDataToTypedOperand :: VariableData -> TypedOperand
variableDataToTypedOperand VariableData {..} = (vdType, vdPtrOperand)

type VariablesMap = M.Map String VariableData

data FunctionData = FunctionData
    { fdRetType  :: Type
    , fdArgTypes :: [Type]
    } deriving (Show)

functionDataToType :: FunctionData -> Type
functionDataToType FunctionData{..} = TFunction fdRetType fdArgTypes

typeToFunctionData :: Type -> Maybe FunctionData
typeToFunctionData (TFunction retType argTypes) =
    Just $ FunctionData retType argTypes
typeToFunctionData _ = Nothing

type FunctionsMap = M.Map String FunctionData

type TypedOperand = (Type, AST.Operand)

-- | ClassVariable type represents variable defined inside class
-- body. It has a type and initial value (used for construction).
data ClassVariable = ClassVariable
    { _cvType        :: Type
    , _cvInitializer :: C.Constant
    } deriving (Show)

$(makeLenses ''ClassVariable)

-- | ClassData stores all data associated with class.
data ClassData = ClassData
    { _cdVariables :: M.Map String ClassVariable
    } deriving (Show)

$(makeLenses ''ClassData)

type ClassesMap = M.Map String ClassData

-- | CodegenState represents all the state used by function body
-- generator.
data CodegenState = CodegenState
    {
      -- ^ Name of the active block to append to.
      _csCurrentBlock :: AST.Name
    ,
      -- ^ Blocks within a function body.
      _csBlocks       :: BlocksMap
    ,
      -- ^ Global functions.
      _csFunctions    :: FunctionsMap
    ,
      -- ^ Global classes.
      _csClasses      :: ClassesMap
    ,
      -- ^ Map from global/local variable name to it's address.
      _csVariables    :: VariablesMap
    ,
      -- ^ Count of unnamed identifiers.
      _csCount        :: Word
    ,
      -- ^ This map is used to generated unique names for blocks.
      _csNames        :: Names
    , _csActiveClass  :: Maybe String  -- ^ Name of class inside which
                                       -- this function is located.
    } deriving (Show)

$(makeLenses ''CodegenState)

-------------------------------------------------------------------------------
-- Codegen
-------------------------------------------------------------------------------

-- | Codegen is a Monad intended to be used for code generation within
-- a single top-level unit like function which is basically a sequence
-- of basic blocks.
newtype Codegen a = Codegen
    { getCodegen :: ExceptT CodegenException (State CodegenState) a
    } deriving (Functor,Applicative,Monad,MonadState CodegenState,MonadError CodegenException)

class ToCodegen a r | a -> r where
    toCodegen :: a -> Codegen r

instance ToCodegen (Codegen a) a where
    toCodegen = id

class HasCodegen s  where
    classesLens :: Lens' s ClassesMap

instance HasCodegen CodegenState where
    classesLens = csClasses

-------------------------------------------------------------------------------
-- Codegen execution
-------------------------------------------------------------------------------

sortBlocks :: [(AST.Name, BlockState)] -> [(AST.Name, BlockState)]
sortBlocks = sortBy (compare `on` (view bsIdx . snd))

createBlocks :: CodegenState -> Either CodegenException [G.BasicBlock]
createBlocks = mapM makeBlock . sortBlocks . M.toList . view csBlocks

makeBlock :: (AST.Name, BlockState) -> Either CodegenException G.BasicBlock
makeBlock (l,(BlockState _ s t)) = G.BasicBlock l s <$> makeTerm t
  where
    makeTerm (Just x) = pure x
    makeTerm Nothing =
        throwCodegenError $
        formatSingle' "block \"{}\" doesn't have terminator" l

entryBlockName :: IsString s => s
entryBlockName = "entry"

emptyBlock :: Word -> BlockState
emptyBlock i = BlockState i [] Nothing

mkCodegen :: FunctionsMap -> ClassesMap -> VariablesMap -> Maybe String -> CodegenState
mkCodegen functions classes symbols activeClass =
    execState (runExceptT . getCodegen $ addBlock entryBlockName) $
    CodegenState
    { _csCurrentBlock = entryBlockName
    , _csBlocks = M.empty
    , _csFunctions = functions
    , _csClasses = classes
    , _csVariables = symbols
    , _csCount = 0
    , _csNames = M.empty
    , _csActiveClass = activeClass
    }

execCodegen :: FunctionsMap
            -> ClassesMap
            -> VariablesMap
            -> Maybe String
            -> Codegen a
            -> Either CodegenException CodegenState
execCodegen functions classes symbols activeClass =
    f . flip runState initial . runExceptT . getCodegen
  where
    initial = mkCodegen functions classes symbols activeClass
    f (Left e,_) = Left e
    f (_,s') = Right s'

-------------------------------------------------------------------------------
-- Basic codegen operations
-------------------------------------------------------------------------------

genUnnamed :: Codegen AST.Name
genUnnamed = do
    csCount += 1
    AST.UnName <$> use csCount

instr :: Type -> AST.Instruction -> Codegen TypedOperand
instr retType ins = do
    name <- genUnnamed
    currentBlock <- getCurrentBlock
    setCurrentBlock $ currentBlock & bsInstructions <>~ [name AST.:= ins]
    return $ (retType, AST.LocalReference (orchidTypeToLLVM retType) name)

terminator :: Named AST.Terminator -> Codegen (AST.Named AST.Terminator)
terminator trm =
    trm <$
    do currentBlock <- getCurrentBlock
       unless (isJust $ currentBlock ^. bsTerm) $
           setCurrentBlock $ currentBlock & bsTerm .~ Just trm

-------------------------------------------------------------------------------
-- Codegen blocks
-------------------------------------------------------------------------------

addBlock :: String -> Codegen AST.Name
addBlock blockName = do
    ix <- fromIntegral . M.size <$> use csBlocks
    names <- use csNames
    let new = emptyBlock ix
        (qname,supply) = uniqueName blockName names
        astName = AST.Name qname
    csBlocks %= M.insert astName new
    csNames .= supply
    return astName

setBlock :: AST.Name -> Codegen ()
setBlock = (csCurrentBlock .=)

-- | Remove block with given name if it exists. Caller is reponsible
-- for ensuring that it is not referenced.
removeBlock :: AST.Name -> Codegen ()
removeBlock name = csBlocks %= M.delete name

getCurrentBlockName :: Codegen AST.Name
getCurrentBlockName = use csCurrentBlock

getCurrentBlock :: Codegen BlockState
getCurrentBlock = do
    v <- M.lookup <$> use csCurrentBlock <*> use csBlocks
    maybe
        (throwCodegenError . formatSingle' "no such block: {}" =<<
         use csCurrentBlock)
        return
        v

isTerminated :: Codegen Bool
isTerminated = isJust . view bsTerm <$> getCurrentBlock

setCurrentBlock :: BlockState -> Codegen ()
setCurrentBlock bs = do
    name <- use csCurrentBlock
    csBlocks %= M.insert name bs

-------------------------------------------------------------------------------
-- Variables, functions and classes
-------------------------------------------------------------------------------

codegenActiveClassData :: Codegen (Maybe ClassData)
codegenActiveClassData =
    maybe (return Nothing) (\n -> use $ csClasses . at n) =<< use csActiveClass

-- | Add variable with given name, type and address.
addVariable :: String -> Type -> TypedOperand -> Codegen ()
addVariable varName varType typedVarAddr@(_,varAddr) = do
    checkTypes "addVariable" [TPointer varType] [typedVarAddr]
    exists <- M.member varName <$> use csVariables
    when exists $ throwCodegenError $
        formatSingle' "variable name is already in scope: {}" varName
    csVariables . at varName .= Just varData
  where
    varData = VariableData varType varAddr

reportNotInScope :: String -> Codegen a
reportNotInScope =
    throwCodegenError . formatSingle' "variable is not in scope: {}"

-- | Get value of variable with given name. Functions are treated as
-- variable too, but function's address is not dereferenced.
lookupName :: String -> Codegen TypedOperand
lookupName var = do
    f <- fmap constructF <$> use (csFunctions . at var)
    v <- maybe (return Nothing) (load >=> return . Just) =<< getPtrMaybe var
    maybe (reportNotInScope var) return $ v <|> f
  where
    constructF fd@FunctionData {..} =
        ( functionDataToType fd
        , AST.ConstantOperand . C.GlobalReference (orchidTypeToLLVM fdRetType) $
          AST.Name var)

-- | Get address of variable with given name. Functions are not
-- considered by this function.
getPtr :: String -> Codegen TypedOperand
getPtr varName =
    maybe (reportNotInScope varName) return =<< getPtrMaybe varName

getPtrMaybe :: String -> Codegen (Maybe TypedOperand)
getPtrMaybe varName =
    (<|>) <$>
    use
        (csVariables .
         at varName . to (fmap (first TPointer . variableDataToTypedOperand))) <*>
    getMemberPtr varName

getMemberPtr :: String -> Codegen (Maybe TypedOperand)
getMemberPtr varName
  | varName == thisPtrName = pure Nothing
  | otherwise =
      maybe (pure Nothing) (getMemberPtrDo varName) =<< codegenActiveClassData

getMemberPtrDo :: String -> ClassData -> Codegen (Maybe TypedOperand)
getMemberPtrDo varName cd = do
    (_, thisPtr) <- getPtr thisPtrName
    case M.lookupIndex varName $ cd ^. cdVariables of
        Nothing -> pure Nothing
        Just i ->
            fmap Just .
            instr (TPointer $ M.elemAt i (cd ^. cdVariables) ^. _2 . cvType) $
            AST.GetElementPtr
                False
                thisPtr
                [ AST.ConstantOperand $ constInt32 0
                , AST.ConstantOperand $ constInt32 $ fromIntegral i]
                []

-------------------------------------------------------------------------------
-- Unary operations
-------------------------------------------------------------------------------

neg :: TypedOperand -> Codegen TypedOperand
neg = sub $ (TInt64, AST.ConstantOperand $ constInt64 0)

not :: TypedOperand -> Codegen TypedOperand
not = xor $ (TBool, AST.ConstantOperand $ constBool True)

-------------------------------------------------------------------------------
-- Binary operations
-------------------------------------------------------------------------------

checkTypes :: String -> [Type] -> [TypedOperand] -> Codegen ()
checkTypes funcName expectedTypes args
  | length expectedTypes /= length args =
      throwCodegenError $
      formatSingle' "{}: incorrect number of arguments" funcName
  | Prelude.and $ zipWith (\t (t',_) -> t == t') expectedTypes args =
      return ()
  | otherwise =
      throwCodegenError $ formatSingle' "{}: incorrect argument type" funcName

or :: TypedOperand -> TypedOperand -> Codegen TypedOperand
or op1@(_,a) op2@(_,b) =
    checkTypes "or" [TBool, TBool] [op1, op2] >> instr TBool (AST.Or a b [])

and :: TypedOperand -> TypedOperand -> Codegen TypedOperand
and op1@(_,a) op2@(_,b) =
    checkTypes "and" [TBool, TBool] [op1, op2] >> instr TBool (AST.And a b [])

xor :: TypedOperand -> TypedOperand -> Codegen TypedOperand
xor op1@(_,a) op2@(_,b) =
    checkTypes "xor" [TBool, TBool] [op1, op2] >> instr TBool (AST.Xor a b [])

cmp :: IP.IntegerPredicate -> TypedOperand -> TypedOperand -> Codegen TypedOperand
cmp cond op1@(_,a) op2@(_,b) =
    checkTypes "cmp" [TInt64, TInt64] [op1, op2] >>
    instr TBool (AST.ICmp cond a b [])

lessThan :: TypedOperand -> TypedOperand -> Codegen TypedOperand
lessThan = cmp IP.SLT

greaterThan :: TypedOperand -> TypedOperand -> Codegen TypedOperand
greaterThan = cmp IP.SGT

equal :: TypedOperand -> TypedOperand -> Codegen TypedOperand
equal = cmp IP.EQ

lessOrEqual :: TypedOperand -> TypedOperand -> Codegen TypedOperand
lessOrEqual = cmp IP.SLE

notEqual :: TypedOperand -> TypedOperand -> Codegen TypedOperand
notEqual = cmp IP.NE

greaterOrEqual :: TypedOperand -> TypedOperand -> Codegen TypedOperand
greaterOrEqual = cmp IP.SGE

add :: TypedOperand -> TypedOperand -> Codegen TypedOperand
add op1@(_,a) op2@(_,b) =
    checkTypes "add" [TInt64, TInt64] [op1, op2] >>
    instr TInt64 (AST.Add False False a b [])

sub :: TypedOperand -> TypedOperand -> Codegen TypedOperand
sub op1@(_,a) op2@(_,b) =
    checkTypes "sub" [TInt64, TInt64] [op1, op2] >>
    instr TInt64 (AST.Sub False False a b [])

mul :: TypedOperand -> TypedOperand -> Codegen TypedOperand
mul op1@(_,a) op2@(_,b) =
    checkTypes "mul" [TInt64, TInt64] [op1, op2] >>
    instr TInt64 (AST.Mul False False a b [])

div :: TypedOperand -> TypedOperand -> Codegen TypedOperand
div op1@(_,a) op2@(_,b) =
    checkTypes "div" [TInt64, TInt64] [op1, op2] >>
    instr TInt64 (AST.SDiv False a b [])

mod :: TypedOperand -> TypedOperand -> Codegen TypedOperand
mod op1@(_,a) op2@(_,b) =
    checkTypes "mod" [TInt64, TInt64] [op1, op2] >> instr TInt64 (AST.SRem a b [])

-------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------

constInt64 :: Int64 -> C.Constant
constInt64 = C.Int 64 . toInteger

-- For internal usage
constInt32 :: Int32 -> C.Constant
constInt32 = C.Int 32 . toInteger

constBool :: Bool -> C.Constant
constBool = C.Int 1 . boolToInteger
  where
    boolToInteger False = 0
    boolToInteger True = 1

class ToConstant a  where
    toConstant
        :: (MonadError CodegenException m, MonadState s m, HasCodegen s)
        => a -> m C.Constant

instance ToConstant Int64 where
    toConstant = pure . constInt64

instance ToConstant Bool where
    toConstant = pure . constBool

-- | Complex constant is produced by constructor.
complexConstant
    :: (MonadError CodegenException m, MonadState s m, HasCodegen s)
    => String -> [(Type, C.Constant)] -> m C.Constant
complexConstant constructorName _ = do
    cls <- use $ classesLens . at constructorName
    maybe onFailure onSuccess cls
  where
    onFailure =
        throwCodegenError $
        formatSingle' "constructor not found: {}" constructorName
    onSuccess =
        return .
        C.Struct Nothing False .
        map (view cvInitializer) . M.elems . view cdVariables

-------------------------------------------------------------------------------
-- Effects
-------------------------------------------------------------------------------

toArgs :: [TypedOperand] -> [(AST.Operand, [A.ParameterAttribute])]
toArgs = map (\(_, x) -> (x, []))

call :: TypedOperand -> [TypedOperand] -> Codegen TypedOperand
call (fnType,fnOperand) args =
    maybe
        (throwCodegenError "attempt to call something that is not a function")
        callDo $
    typeToFunctionData fnType
  where
    callDo FunctionData{..} = do
        checkTypes "call" fdArgTypes args
        instr fdRetType $
            AST.Call Nothing CC.C [] (Right fnOperand) (toArgs args) [] []

alloca :: Type -> Codegen TypedOperand
alloca ty = instr (TPointer ty) $ AST.Alloca (orchidTypeToLLVM ty) Nothing 0 []

store :: TypedOperand -> TypedOperand -> Codegen ()
store (ptrType,ptr) (valType,val) = do
    unless (ptrType == TPointer valType) $
        throwCodegenError $
        format'
            "can't store value of type {} to the pointer of type {}"
            (show valType, show ptrType)
    () <$ instr TVoid (AST.Store False ptr val Nothing 0 [])

load :: TypedOperand -> Codegen TypedOperand
load (TPointer t, ptr) = instr t $ AST.Load False ptr Nothing 0 []
load (t,_) =
    throwCodegenError $
    formatSingle' "value of type {} was given to load" $ show t

-------------------------------------------------------------------------------
-- Control Flow
-------------------------------------------------------------------------------

br :: AST.Name -> Codegen (AST.Named AST.Terminator)
br = terminator . AST.Do . flip AST.Br []

cbr :: TypedOperand -> AST.Name -> AST.Name -> Codegen (AST.Named AST.Terminator)
cbr c@(_,cond) tr fl =
    checkTypes "cbr" [TBool] [c] >>
    (terminator $ AST.Do $ AST.CondBr cond tr fl [])

-- phi :: Type -> [(AST.Operand, AST.Name)] -> Codegen AST.Operand
-- phi ty incoming = instr $ AST.Phi ty incoming []

ret :: Maybe AST.Operand -> Codegen (AST.Named AST.Terminator)
ret = terminator . AST.Do . flip AST.Ret []

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

predefinedTypes :: M.Map String Type
predefinedTypes = M.fromList [("int64", TInt64), ("bool", TBool)]

classType :: String -> ClassData -> Type
classType className =
    TClass (convertString className) .
    map (view cvType) . M.elems . view cdVariables

lookupType
    :: (MonadError CodegenException m, MonadState s m, HasCodegen s)
    => String -> m Type
lookupType t = maybe tryClass return $ M.lookup t predefinedTypes
  where
    throwUnknownType = throwCodegenError $ formatSingle' "unknown type: {}" t
    tryClass =
        maybe throwUnknownType (return . classType t) =<< use (classesLens . at t)

classPointerType
    :: (MonadError CodegenException m, MonadState s m, HasCodegen s)
    => String -> m Type
classPointerType t =
    maybe
        throwUnknownType
        (return . TPointer  . classType t) =<<
    use (classesLens . at t)
  where
    throwUnknownType = throwCodegenError $ formatSingle' "unknown type: {}" t

-------------------------------------------------------------------------------
-- Module Level
-------------------------------------------------------------------------------

$(makeLensesFor [("moduleDefinitions", "mDefinitions")] ''AST.Module)

-- | ModuleState is a state of the whole module.
data ModuleState = ModuleState
    { _msFunctions :: FunctionsMap
    , _msClasses   :: ClassesMap
    , _msVariables :: VariablesMap
    , _msClass     :: Maybe String
    , _msModule    :: AST.Module
    } deriving (Show)

$(makeLenses ''ModuleState)

instance HasCodegen ModuleState where
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
    :: String
    -> AST.Module
    -> FunctionsMap
    -> ClassesMap
    -> VariablesMap
    -> ModuleState
mkModuleState moduleName preludeModule preludeFunctions preludeClasses preludeVariables =
    ModuleState
    { _msFunctions = preludeFunctions
    , _msClasses = preludeClasses
    , _msVariables = preludeVariables
    , _msClass = Nothing
    , _msModule = preludeModule
      { AST.moduleName = moduleName
      }
    }

-- | Get name of active class (if any).
getActiveClass :: LLVM (Maybe String)
getActiveClass = use msClass

addDefn :: AST.Definition -> LLVM ()
addDefn d = msModule . mDefinitions <>= [d]

-- | Add function with given return type, name, arguments and suite to
-- module. This function sets active class for given codegen.
addFuncDef
    :: (ToCodegen a r)
    => Type -> String -> [(Type, AST.Name)] -> a -> LLVM ()
addFuncDef retType funcName args suite = do
    funcs <- use msFunctions
    classes <- use msClasses
    vars <- use msVariables
    activeClass <- use msClass
    case bodyEither funcs classes vars activeClass of
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
    funcName' = AST.Name funcName
    funcData =
        FunctionData
        { fdRetType = retType
        , fdArgTypes = map fst args
        }
    bodyEither funcs classes vars activeClass =
        execCodegen funcs classes vars activeClass (toCodegen suite) >>=
        createBlocks
    parameters = ([G.Parameter (orchidTypeToLLVM t) n [] | (t,n) <- args], False)

-- | Outside class this function adds global variable with given type
-- and name to module. Inside class definition it adds given variable
-- as part of class.
addGlobalVariable
    :: (ToConstant a)
    => Type -> String -> a -> LLVM ()
addGlobalVariable varType varName varExpr = do
    value <- toConstant varExpr
    cls <- use msClass
    maybe addToGlobal addToClass cls $ value
  where
    varName' = AST.Name varName
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
    addToClass cls value = do
        msClasses . at cls . _Just . cdVariables %=
            (M.insert varName (clsVarData value))
    clsVarData value =
        ClassVariable
        { _cvType = varType
        , _cvInitializer = value
        }

startClassDef :: String -> LLVM ()
startClassDef className = do
    cls <- use msClass
    maybe
        addNewCls
        (const $
         throwCodegenError "class definition can't appear inside another class")
        cls
  where
    newClass =
        ClassData
        { _cdVariables = M.empty
        }
    addNewCls = do
        msClass .= Just className
        msClasses %= M.insert className newClass

finishClassDef :: LLVM ()
finishClassDef = maybe reportError (const $ msClass .= Nothing) =<< use msClass
  where
    reportError =
        throwCodegenError
            "internal error: attempt to finish definition of class outside of class"
