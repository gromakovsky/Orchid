{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE TemplateHaskell        #-}

-- | Function body code generation.

module Orchid.Codegen.Body
       (
         -- Codegen
         BodyGen
       , ToBody (toBody)
       , ToBodyPtr (toBodyPtr)
       , createBlocks
       , execBodyGen

         -- Symbol table
       , addVariable
       , lookupName
       , lookupMember
       , getPtr
       , getMemberPtr

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

         -- Effects
       , lowLevelCast
       , call
       , alloca
       , store
       , load

         -- Control Flow
       , br
       , cbr
       , ret
       ) where

import           Control.Applicative                (empty, (<|>))
import           Control.Lens                       (at, makeLenses, to, use,
                                                     view, (%=), (+=), (.=),
                                                     (.~), (<>~), (^.), _2)
import           Control.Monad                      (unless, when, (>=>))
import           Control.Monad.Except               (ExceptT,
                                                     MonadError (catchError),
                                                     runExceptT)
import           Control.Monad.State                (MonadState, State,
                                                     execState, runState)
import           Data.Bifunctor                     (first)
import           Data.Function                      (on, (&))
import           Data.List                          (sortBy)
import qualified Data.Map                           as M
import           Data.Maybe                         (isJust)
import qualified Data.Set                           as S
import           Data.String                        (IsString)
import           Data.String.Conversions            (convertString)
import           Data.Text                          (Text)
import qualified LLVM.General.AST                   as AST
import qualified LLVM.General.AST.Attribute         as A
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Constant          as C
import qualified LLVM.General.AST.Global            as G
import qualified LLVM.General.AST.IntegerPredicate  as IP
import           Prelude                            hiding (and, div, mod, not,
                                                     or)
import qualified Prelude

import           Serokell.Util                      (format', formatSingle',
                                                     listBuilderJSON)

import           Orchid.Codegen.Common              (ClassData, ClassesMap, FunctionData (FunctionData),
                                                     FunctionsMap,
                                                     HasClasses (classesLens),
                                                     Names, TypedOperand, VariableData (VariableData),
                                                     VariablesMap, cdVariables,
                                                     classAndParents, cvPrivate,
                                                     cvType, fdArgTypes,
                                                     fdRetType,
                                                     functionDataToType,
                                                     isSubClass,
                                                     mangleClassMethodName,
                                                     orchidTypeToLLVM,
                                                     thisPtrName,
                                                     throwCodegenError,
                                                     typeToFunctionData,
                                                     uniqueName,
                                                     variableDataToTypedOperand)
import           Orchid.Codegen.Constant            (constBool, constInt32,
                                                     constInt64)
import           Orchid.Codegen.Type                (Type (..))
import           Orchid.Error                       (CodegenException)

type Named = AST.Named

-- | BlockState corresponds to LLVM's BasicBlock. It is constructed
-- step-by-step.
data BlockState = BlockState
    {
      -- | Block has an index which helps to sort blocks after they
      -- are constructed.
      _bsIdx          :: Word
    ,
      -- | Block contains ordered list of instructions which is
      -- initially empty and is populated step-by-step.
      _bsInstructions :: [Named AST.Instruction]
    ,
      -- | Block must contain a terminator. This field is optional,
      -- because block is constructed using iterative process.
      _bsTerm         :: Maybe (Named AST.Terminator)
    } deriving (Show)

$(makeLenses ''BlockState)

type BlocksMap = M.Map AST.Name BlockState

-------------------------------------------------------------------------------
-- Body State
-------------------------------------------------------------------------------

-- | BodyState represents all the state used by function body
-- generator.
data BodyState = BodyState
    {
      -- | Name of the active block to append to.
      _bsCurrentBlock     :: AST.Name
    ,
      -- | Blocks within a function body.
      _bsBlocks           :: BlocksMap
    ,
      -- | Global functions.
      _bsFunctions        :: FunctionsMap
    ,
      -- | Private functions.
      _bsPrivateFunctions :: S.Set Text
    ,
      -- | Global classes.
      _bsClasses          :: ClassesMap
    ,
      -- | Map from global/local variable name to it's address.
      _bsVariables        :: VariablesMap
    ,
      -- | Count of unnamed identifiers.
      _bsCount            :: Word
    ,
      -- | This map is used to generated unique names for blocks.
      _bsNames            :: Names
      -- | Name of class inside which this function is located
    , _bsActiveClass      :: Maybe Text
    } deriving (Show)

$(makeLenses ''BodyState)

instance HasClasses BodyState where
    classesLens = bsClasses

-------------------------------------------------------------------------------
-- BodyGen
-------------------------------------------------------------------------------

-- | BodyGen is a Monad intended to be used for body generation of
-- a single top-level unit like function which is basically a sequence
-- of basic blocks.
newtype BodyGen a = BodyGen
    { getBodyGen :: ExceptT CodegenException (State BodyState) a
    } deriving (Functor,Applicative,Monad,MonadState BodyState,MonadError CodegenException)

class ToBody a r | a -> r where
    toBody :: a -> BodyGen r

instance ToBody (BodyGen a) a where
    toBody = id

-- | Using this class one can create a TypedOperand which is a pointer to lvalue.
class ToBodyPtr a where
    toBodyPtr :: a -> BodyGen TypedOperand

-------------------------------------------------------------------------------
-- BodyGen execution
-------------------------------------------------------------------------------

sortBlocks :: [(AST.Name, BlockState)] -> [(AST.Name, BlockState)]
sortBlocks = sortBy (compare `on` (view bsIdx . snd))

createBlocks :: BodyState -> Either CodegenException [G.BasicBlock]
createBlocks = mapM makeBlock . sortBlocks . M.toList . view bsBlocks

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

mkBodyState
    :: FunctionsMap
    -> S.Set Text
    -> ClassesMap
    -> VariablesMap
    -> Maybe Text
    -> BodyState
mkBodyState functions privateFunctions classes symbols activeClass =
    execState (runExceptT . getBodyGen $ addBlock entryBlockName) $
    BodyState
    { _bsCurrentBlock = entryBlockName
    , _bsBlocks = M.empty
    , _bsFunctions = functions
    , _bsPrivateFunctions = privateFunctions
    , _bsClasses = classes
    , _bsVariables = symbols
    , _bsCount = 0
    , _bsNames = M.empty
    , _bsActiveClass = activeClass
    }

execBodyGen
    :: FunctionsMap
    -> S.Set Text
    -> ClassesMap
    -> VariablesMap
    -> Maybe Text
    -> BodyGen a
    -> Either CodegenException BodyState
execBodyGen functions privateFunctions classes symbols activeClass =
    f . flip runState initial . runExceptT . getBodyGen
  where
    initial =
        mkBodyState functions privateFunctions classes symbols activeClass
    f (Left e,_) = Left e
    f (_,s') = Right s'

-------------------------------------------------------------------------------
-- Basic BodyGen operations
-------------------------------------------------------------------------------

genUnnamed :: BodyGen AST.Name
genUnnamed = do
    bsCount += 1
    AST.UnName <$> use bsCount

instr :: Type -> AST.Instruction -> BodyGen TypedOperand
instr retType ins = do
    name <- genUnnamed
    currentBlock <- getCurrentBlock
    setCurrentBlock $ currentBlock & bsInstructions <>~ [name AST.:= ins]
    return $ (retType, AST.LocalReference (orchidTypeToLLVM retType) name)

terminator :: Named AST.Terminator -> BodyGen (AST.Named AST.Terminator)
terminator trm =
    trm <$
    do currentBlock <- getCurrentBlock
       unless (isJust $ currentBlock ^. bsTerm) $
           setCurrentBlock $ currentBlock & bsTerm .~ Just trm

-------------------------------------------------------------------------------
-- BodyGen blocks
-------------------------------------------------------------------------------

addBlock :: Text -> BodyGen AST.Name
addBlock blockName = do
    ix <- fromIntegral . M.size <$> use bsBlocks
    names <- use bsNames
    let new = emptyBlock ix
        (qname,supply) = uniqueName blockName names
        astName = convertString qname
    bsBlocks %= M.insert astName new
    bsNames .= supply
    return astName

setBlock :: AST.Name -> BodyGen ()
setBlock = (bsCurrentBlock .=)

-- | Remove block with given name if it exists. Caller is reponsible
-- for ensuring that it is not referenced.
removeBlock :: AST.Name -> BodyGen ()
removeBlock name = bsBlocks %= M.delete name

getCurrentBlockName :: BodyGen AST.Name
getCurrentBlockName = use bsCurrentBlock

getCurrentBlock :: BodyGen BlockState
getCurrentBlock = do
    v <- M.lookup <$> use bsCurrentBlock <*> use bsBlocks
    maybe
        (throwCodegenError . formatSingle' "no such block: {}" =<<
         use bsCurrentBlock)
        return
        v

isTerminated :: BodyGen Bool
isTerminated = isJust . view bsTerm <$> getCurrentBlock

setCurrentBlock :: BlockState -> BodyGen ()
setCurrentBlock bs = do
    name <- use bsCurrentBlock
    bsBlocks %= M.insert name bs

-------------------------------------------------------------------------------
-- Variables, functions and classes
-------------------------------------------------------------------------------

codegenActiveClassData :: BodyGen (Maybe ClassData)
codegenActiveClassData =
    maybe (return Nothing) (\n -> use $ bsClasses . at n) =<< use bsActiveClass

-- | Add variable with given name, type and address.
addVariable :: Text -> Type -> TypedOperand -> BodyGen ()
addVariable varName varType typedVarAddr@(_,varAddr) = do
    checkTypes "addVariable" [TPointer varType] [typedVarAddr]
    exists <- M.member varName <$> use bsVariables
    when exists $ throwCodegenError $
        formatSingle' "variable name is already in scope: {}" varName
    bsVariables . at varName .= Just varData
  where
    varData = VariableData varType varAddr

reportNotInScope :: Text -> BodyGen a
reportNotInScope =
    throwCodegenError . formatSingle' "not in scope: {}"

reportClassNotFound :: Text -> BodyGen a
reportClassNotFound =
    throwCodegenError . formatSingle' "no such class {}"

-- | Get value of variable with given name. Functions are treated as
-- variable too, but function's address is not dereferenced.
lookupName :: Text -> BodyGen TypedOperand
lookupName var = do
    f <- constructF True var
    classes <- classAndParents =<< use bsActiveClass
    methods <- mapM constructMethod classes
    v <- maybe (return Nothing) (load >=> return . Just) =<< getPtrMaybe var
    maybe (reportNotInScope var) return $
        foldr (<|>) empty (v : methods ++ [f])
  where
    constructF considerPrivate name = do
        isPrivate <- S.member name <$> use bsPrivateFunctions
        if isPrivate && considerPrivate
            then return Nothing
            else fmap (constructFDo name) <$> use (bsFunctions . at name)
    constructFDo name fd@FunctionData{..} =
        ( functionDataToType fd
        , AST.ConstantOperand . C.GlobalReference (orchidTypeToLLVM fdRetType) $
          convertString name)
    constructMethod className =
        constructF False $ mangleClassMethodName className var

lookupMember :: TypedOperand -> Text -> BodyGen TypedOperand
lookupMember operand memberName = getMemberPtr operand memberName >>= load

-- | Get address of variable with given name. Functions are not
-- considered by this function.
getPtr :: Text -> BodyGen TypedOperand
getPtr varName =
    maybe (reportNotInScope varName) return =<< getPtrMaybe varName

getMemberPtr :: TypedOperand -> Text -> BodyGen TypedOperand
getMemberPtr op@(TPointer (TClass className _),_) varName = do
    classDataMaybe <- use (bsClasses . at className)
    maybe
        (reportClassNotFound className)
        (\cd ->
              maybe (reportNotInScope varName) return =<<
              getMemberPtrMaybe True op varName cd)
        classDataMaybe
getMemberPtr (t,_) _ =
    throwCodegenError $
    formatSingle'
        "can't access member of type which is not a pointer to class {}"
        t

getMemberPtrMaybe :: Bool
                  -> TypedOperand
                  -> Text
                  -> ClassData
                  -> BodyGen (Maybe TypedOperand)
getMemberPtrMaybe considerPrivate (TPointer (TClass _ _),ptrOperand) varName cd =
    case M.lookupIndex varName $ cd ^. cdVariables of
        Nothing -> pure Nothing
        Just i -> do
            let classVariable = M.elemAt i (cd ^. cdVariables) ^. _2
            when (considerPrivate && classVariable ^. cvPrivate) $
                throwCodegenError $
                formatSingle' "variable {} is private" varName
            fmap Just . instr (TPointer $ classVariable ^. cvType) $
                AST.GetElementPtr
                    False
                    ptrOperand
                    [ AST.ConstantOperand $ constInt32 0
                    , AST.ConstantOperand $ constInt32 $ fromIntegral i]
                    []
getMemberPtrMaybe _ _ _ _ = pure Nothing

getPtrMaybe :: Text -> BodyGen (Maybe TypedOperand)
getPtrMaybe varName =
    (<|>) <$>
    use
        (bsVariables .
         at varName . to (fmap (first TPointer . variableDataToTypedOperand))) <*>
    getThisMemberPtr varName

getThisMemberPtr :: Text -> BodyGen (Maybe TypedOperand)
getThisMemberPtr varName
  | varName == thisPtrName = pure Nothing
  | otherwise = do
      thisPtr <- getPtrMaybe thisPtrName
      case thisPtr of
          Nothing -> pure Nothing
          Just ptr ->
              maybe (pure Nothing) (getMemberPtrMaybe False ptr varName) =<<
              codegenActiveClassData

-------------------------------------------------------------------------------
-- Unary operations
-------------------------------------------------------------------------------

neg, not :: TypedOperand -> BodyGen TypedOperand
neg = sub $ (TInt64, AST.ConstantOperand $ constInt64 0)
not = xor $ (TBool, AST.ConstantOperand $ constBool True)

-------------------------------------------------------------------------------
-- Binary operations
-------------------------------------------------------------------------------

checkTypes :: Text -> [Type] -> [TypedOperand] -> BodyGen ()
checkTypes funcName expectedTypes args
  | length expectedTypes /= length args =
      throwCodegenError $
      format' "{}: incorrect number of arguments{}" (funcName, endMsg)
  | Prelude.and $
        zipWith
            (\t (t',_) ->
                  t == t')
            expectedTypes
            args =
      return ()
  | otherwise =
      throwCodegenError $
      format' "{}: incorrect argument type{}" (funcName, endMsg)
  where
    endMsg =
        format' ", expected types: {}, received types: {}"
            (listBuilderJSON expectedTypes, listBuilderJSON $ map fst args)

or, and, xor :: TypedOperand -> TypedOperand -> BodyGen TypedOperand
or op1@(_,a) op2@(_,b) =
    checkTypes "or" [TBool, TBool] [op1, op2] >> instr TBool (AST.Or a b [])
and op1@(_,a) op2@(_,b) =
    checkTypes "and" [TBool, TBool] [op1, op2] >> instr TBool (AST.And a b [])
xor op1@(_,a) op2@(_,b) =
    checkTypes "xor" [TBool, TBool] [op1, op2] >> instr TBool (AST.Xor a b [])

cmp :: IP.IntegerPredicate -> TypedOperand -> TypedOperand -> BodyGen TypedOperand
cmp cond op1@(_,a) op2@(_,b) =
    checkTypes "cmp" [TInt64, TInt64] [op1, op2] >>
    instr TBool (AST.ICmp cond a b [])

lessThan, greaterThan, equal, lessOrEqual, notEqual, greaterOrEqual :: TypedOperand -> TypedOperand -> BodyGen TypedOperand
lessThan = cmp IP.SLT
greaterThan = cmp IP.SGT
equal = cmp IP.EQ
lessOrEqual = cmp IP.SLE
notEqual = cmp IP.NE
greaterOrEqual = cmp IP.SGE

add, sub, mul, div, mod :: TypedOperand -> TypedOperand -> BodyGen TypedOperand
add op1@(_,a) op2@(_,b) =
    checkTypes "add" [TInt64, TInt64] [op1, op2] >>
    instr TInt64 (AST.Add False False a b [])
sub op1@(_,a) op2@(_,b) =
    checkTypes "sub" [TInt64, TInt64] [op1, op2] >>
    instr TInt64 (AST.Sub False False a b [])
mul op1@(_,a) op2@(_,b) =
    checkTypes "mul" [TInt64, TInt64] [op1, op2] >>
    instr TInt64 (AST.Mul False False a b [])
div op1@(_,a) op2@(_,b) =
    checkTypes "div" [TInt64, TInt64] [op1, op2] >>
    instr TInt64 (AST.SDiv False a b [])
mod op1@(_,a) op2@(_,b) =
    checkTypes "mod" [TInt64, TInt64] [op1, op2] >> instr TInt64 (AST.SRem a b [])

-------------------------------------------------------------------------------
-- Effects
-------------------------------------------------------------------------------

toArgs :: [TypedOperand] -> [(AST.Operand, [A.ParameterAttribute])]
toArgs = map (\(_, x) -> (x, []))

lowLevelCast :: AST.Operand -> Type -> BodyGen TypedOperand
lowLevelCast op newType = instr newType $ AST.BitCast op (orchidTypeToLLVM newType) []

cast :: TypedOperand -> Type -> BodyGen TypedOperand
cast (t1@(TPointer (TClass origClass _)),origOperand) newType@(TPointer (TClass newClass _)) = do
    isSC <- isSubClass origClass newClass
    unless isSC $
        throwCodegenError $
        format' "cast from {} to {} is prohibited" (t1, newType)
    lowLevelCast origOperand newType
cast (t1,origOperand) t2
  | t1 == t2 = pure (t1, origOperand)
  | otherwise =
      throwCodegenError $ format' "cast from {} to {} is prohibited" (t1, t2)

castArgs :: [TypedOperand] -> [Type] -> BodyGen [TypedOperand]
castArgs args expectedTypes
  | length args /= length expectedTypes = pure args
  | otherwise = mapM castArgsDo $ zip args expectedTypes

castArgsDo :: (TypedOperand, Type) -> BodyGen TypedOperand
castArgsDo (operand, t) = cast operand t `catchError` const (pure operand)

call :: TypedOperand -> [TypedOperand] -> BodyGen TypedOperand
call (fnType,fnOperand) args =
    maybe
        (throwCodegenError "attempt to call something that is not a function")
        callDo $
    typeToFunctionData fnType
  where
    callDo FunctionData{..} = do
        activeClassName <- use bsActiveClass
        args' <-
            maybePrependThis activeClassName fdArgTypes >>=
            flip castArgs fdArgTypes
        checkTypes "call" fdArgTypes args'
        instr fdRetType $
            AST.Call Nothing CC.C [] (Right fnOperand) (toArgs args') [] []
    maybePrependThis Nothing _ = pure args
    maybePrependThis (Just className) argTypes
      | length argTypes /= length args + 1 = pure args
      | otherwise =
          case unwrapClassPtr (head argTypes) of
              Nothing -> pure args
              Just expectedType -> do
                  isSC <- isSubClass className expectedType
                  if isSC
                      then (: args) <$>
                           (getPtr thisPtrName >>= flip cast (head argTypes))
                      else pure args
    unwrapClassPtr (TPointer (TClass n _)) = Just n
    unwrapClassPtr _ = Nothing

alloca :: Type -> BodyGen TypedOperand
alloca ty = instr (TPointer ty) $ AST.Alloca (orchidTypeToLLVM ty) Nothing 0 []

store :: TypedOperand -> TypedOperand -> BodyGen ()
store (ptrType,ptr) (valType,val) = do
    unless (ptrType == TPointer valType) $
        throwCodegenError $
        format'
            "can't store value of type {} to the pointer of type {}"
            (valType, ptrType)
    () <$ instr TVoid (AST.Store False ptr val Nothing 0 [])

load :: TypedOperand -> BodyGen TypedOperand
load (TPointer t, ptr) = instr t $ AST.Load False ptr Nothing 0 []
load (t,_) =
    throwCodegenError $
    formatSingle' "value of type {} was given to load" t

-------------------------------------------------------------------------------
-- Control Flow
-------------------------------------------------------------------------------

br :: AST.Name -> BodyGen (AST.Named AST.Terminator)
br = terminator . AST.Do . flip AST.Br []

cbr :: TypedOperand -> AST.Name -> AST.Name -> BodyGen (AST.Named AST.Terminator)
cbr c@(_,cond) tr fl =
    checkTypes "cbr" [TBool] [c] >>
    (terminator $ AST.Do $ AST.CondBr cond tr fl [])

ret :: Maybe AST.Operand -> BodyGen (AST.Named AST.Terminator)
ret = terminator . AST.Do . flip AST.Ret []
