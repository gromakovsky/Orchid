{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

-- | Primitive functions to work with LLVM code generation.

module Orchid.Codegen
       (
         -- Helpers
         throwCodegenError

         -- Types
       , int64
       , bool

         -- Codegen
       , Codegen
       , ToCodegen
       , toCodegen
       , createBlocks
       , execCodegen

         -- Symbol table
       , addVariable
       , getValue
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

         -- Effects
       , call
       , alloca
       , store
       , load

         -- Control Flow
       , br
       , cbr
       , phi
       , ret

         -- Module level
       , ModuleState
       , LLVM
       , ToLLVM
       , toLLVM
       , execLLVM
       , mkModuleState
       , addFuncDef
       , addGlobalVariable

         -- Reexports
       , Type (..)
       , void
       , AST.Name (..)
       ) where

import           Control.Applicative                ((<|>))
import           Control.Lens                       (at, makeLenses,
                                                     makeLensesFor, use, view,
                                                     (%=), (+=), (.=), (.~),
                                                     (<>=), (<>~), (^.))
import           Control.Monad                      (unless, when, (>=>))
import           Control.Monad.Except               (ExceptT,
                                                     MonadError (throwError),
                                                     runExceptT)
import           Control.Monad.State                (MonadState, State,
                                                     execState, runState)
import           Data.Function                      (on, (&))
import           Data.Int                           (Int64)
import           Data.List                          (sortBy)
import qualified Data.Map                           as M
import           Data.Maybe                         (isJust)
import           Data.String                        (IsString (fromString))
import           Data.Text                          (Text)
import           Data.Text.Buildable                (Buildable (build))
import qualified LLVM.General.AST                   as AST
import qualified LLVM.General.AST.Attribute         as A
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Constant          as C
import qualified LLVM.General.AST.Global            as G
import qualified LLVM.General.AST.IntegerPredicate  as IP
import           LLVM.General.AST.Type              (Type (..), i1, i64, void)
import           Prelude                            hiding (and, div, mod, not,
                                                     or)
import           Serokell.Util                      (formatSingle')

import           Orchid.Error                       (CodegenException (CodegenException))

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

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

int64, bool :: Type
int64 = i64
bool = i1

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

type BlockMap = M.Map AST.Name BlockState

-- | FIXME: add type
type VariableData = AST.Operand
type VariablesMap = M.Map String VariableData

-- | FIXME: add argument types
type FunctionData = Type
type FunctionsMap = M.Map String FunctionData

-- | CodegenState represents all the state used by function body
-- generator.
data CodegenState = CodegenState
    {
      -- ^ Name of the active block to append to.
      _csCurrentBlock :: AST.Name
    ,
      -- ^ Blocks within a function body.
      _csBlocks       :: BlockMap
    ,
      -- ^ Global functions.
      _csFunctions    :: FunctionsMap
    ,
      -- ^ Map from global/local variable name to it's address.
      _csVariables    :: VariablesMap
    ,
      -- ^ Count of unnamed identifiers.
      _csCount        :: Word
    ,
      -- ^ This map is used to generated unique names for blocks.
      _csNames        :: Names
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

emptyCodegen :: FunctionsMap -> VariablesMap -> CodegenState
emptyCodegen functions symbols =
    execState (runExceptT . getCodegen $ addBlock entryBlockName) $
    CodegenState
    { _csCurrentBlock = entryBlockName
    , _csBlocks = M.empty
    , _csFunctions = functions
    , _csVariables = symbols
    , _csCount = 0
    , _csNames = M.empty
    }

execCodegen :: FunctionsMap
            -> VariablesMap
            -> Codegen a
            -> Either CodegenException CodegenState
execCodegen functions symbols = f . flip runState initial . runExceptT . getCodegen
  where
    initial = emptyCodegen functions symbols
    f (Left e,_) = Left e
    f (_,s') = Right s'

-------------------------------------------------------------------------------
-- Basic codegen operations
-------------------------------------------------------------------------------

genUnnamed :: Codegen AST.Name
genUnnamed = do
    csCount += 1
    AST.UnName <$> use csCount

instr :: AST.Instruction -> Codegen AST.Operand
instr ins = do
    name <- genUnnamed
    currentBlock <- getCurrentBlock
    setCurrentBlock $ currentBlock & bsInstructions <>~ [name AST.:= ins]
    -- FIXME: pass type somehow
    return $ AST.LocalReference int64 name

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
-- Variables and functions
-------------------------------------------------------------------------------

-- | Add variable with given name and address.
addVariable :: String -> AST.Operand -> Codegen ()
addVariable varName varAddr = do
    exists <- M.member varName <$> use csVariables
    when exists $ throwCodegenError $
        formatSingle' "variable name is already in scope: {}" varName
    csVariables . at varName .= Just varAddr

reportNotInScope :: String -> Codegen a
reportNotInScope =
    throwCodegenError . formatSingle' "variable is not is scope: {}"

-- | Get value of variable with given name. Functions are treated as
-- variable too, but function's address is not dereferenced.
getValue :: String -> Codegen AST.Operand
getValue var = do
    f <- fmap constructF <$> use (csFunctions . at var)
    v <-
        maybe (return Nothing) (load >=> return . Just) =<<
        use (csVariables . at var)
    maybe (reportNotInScope var) return $ v <|> f
  where
    constructF t = AST.ConstantOperand . C.GlobalReference t $ AST.Name var

-- | Get address of variable with given name. Functions are not
-- considered by this function.
getPtr :: String -> Codegen AST.Operand
getPtr varName =
    maybe (reportNotInScope varName) return =<< use (csVariables . at varName)

-------------------------------------------------------------------------------
-- Unary operations
-------------------------------------------------------------------------------

neg :: AST.Operand -> Codegen AST.Operand
neg = sub $ AST.ConstantOperand $ constInt64 0

not :: AST.Operand -> Codegen AST.Operand
not = xor $ AST.ConstantOperand $ constBool True

-------------------------------------------------------------------------------
-- Binary operations
-------------------------------------------------------------------------------

or :: AST.Operand -> AST.Operand -> Codegen AST.Operand
or a b = instr $ AST.Or a b []

and :: AST.Operand -> AST.Operand -> Codegen AST.Operand
and a b = instr $ AST.And a b []

xor :: AST.Operand -> AST.Operand -> Codegen AST.Operand
xor a b = instr $ AST.Xor a b []

cmp :: IP.IntegerPredicate -> AST.Operand -> AST.Operand -> Codegen AST.Operand
cmp cond a b = instr $ AST.ICmp cond a b []

lessThan :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lessThan = cmp IP.SLT

greaterThan :: AST.Operand -> AST.Operand -> Codegen AST.Operand
greaterThan = cmp IP.SGT

equal :: AST.Operand -> AST.Operand -> Codegen AST.Operand
equal = cmp IP.EQ

lessOrEqual :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lessOrEqual = cmp IP.SLE

notEqual :: AST.Operand -> AST.Operand -> Codegen AST.Operand
notEqual = cmp IP.NE

greaterOrEqual :: AST.Operand -> AST.Operand -> Codegen AST.Operand
greaterOrEqual = cmp IP.SGE

add :: AST.Operand -> AST.Operand -> Codegen AST.Operand
add a b = instr $ AST.Add False False a b []

sub :: AST.Operand -> AST.Operand -> Codegen AST.Operand
sub a b = instr $ AST.Sub False False a b []

mul :: AST.Operand -> AST.Operand -> Codegen AST.Operand
mul a b = instr $ AST.Mul False False a b []

div :: AST.Operand -> AST.Operand -> Codegen AST.Operand
div a b = instr $ AST.SDiv False a b []

mod :: AST.Operand -> AST.Operand -> Codegen AST.Operand
mod a b = instr $ AST.SRem a b []

-------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------

constInt64 :: Int64 -> C.Constant
constInt64 = C.Int 64 . toInteger

constBool :: Bool -> C.Constant
constBool = C.Int 1 . boolToInteger
  where
    boolToInteger False = 0
    boolToInteger True = 1

class ToConstant a  where
    toConstant
        :: MonadError CodegenException m
        => a -> m C.Constant

instance ToConstant Int64 where
    toConstant = pure . constInt64

instance ToConstant Bool where
    toConstant = pure . constBool

-------------------------------------------------------------------------------
-- Effects
-------------------------------------------------------------------------------

toArgs :: [AST.Operand] -> [(AST.Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

call :: AST.Operand -> [AST.Operand] -> Codegen AST.Operand
call fn args = instr $ AST.Call Nothing CC.C [] (Right fn) (toArgs args) [] []

alloca :: Type -> Codegen AST.Operand
alloca ty = instr $ AST.Alloca ty Nothing 0 []

store :: AST.Operand -> AST.Operand -> Codegen AST.Operand
store ptr val = instr $ AST.Store False ptr val Nothing 0 []

load :: AST.Operand -> Codegen AST.Operand
load ptr = instr $ AST.Load False ptr Nothing 0 []

-------------------------------------------------------------------------------
-- Control Flow
-------------------------------------------------------------------------------

br :: AST.Name -> Codegen (AST.Named AST.Terminator)
br = terminator . AST.Do . flip AST.Br []

cbr :: AST.Operand -> AST.Name -> AST.Name -> Codegen (AST.Named AST.Terminator)
cbr cond tr fl = terminator $ AST.Do $ AST.CondBr cond tr fl []

phi :: Type -> [(AST.Operand, AST.Name)] -> Codegen AST.Operand
phi ty incoming = instr $ AST.Phi ty incoming []

ret :: Maybe AST.Operand -> Codegen (AST.Named AST.Terminator)
ret = terminator . AST.Do . flip AST.Ret []

-------------------------------------------------------------------------------
-- Module Level
-------------------------------------------------------------------------------

$(makeLensesFor [("moduleDefinitions", "mDefinitions")] ''AST.Module)

-- | ModuleState is a state of the whole module.
data ModuleState = ModuleState
    { _msFunctions :: FunctionsMap
    , _msVariables :: VariablesMap
    , _msModule    :: AST.Module
    } deriving (Show)

$(makeLenses ''ModuleState)

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
mkModuleState :: String -> AST.Module -> FunctionsMap -> VariablesMap -> ModuleState
mkModuleState moduleName preludeModule preludeFunctions preludeVariables =
    ModuleState
    { _msFunctions = preludeFunctions
    , _msVariables = preludeVariables
    , _msModule = preludeModule
      { AST.moduleName = moduleName
      }
    }

addDefn :: AST.Definition -> LLVM ()
addDefn d = msModule . mDefinitions <>= [d]

-- | Add function with given return type, name, arguments and suite to
-- module.
addFuncDef
    :: (ToCodegen a r)
    => AST.Type -> String -> [(AST.Type, AST.Name)] -> a -> LLVM ()
addFuncDef retType funcName args suite = do
    funcs <- use msFunctions
    vars <- use msVariables
    case bodyEither funcs vars of
        Left e -> throwError e
        Right body -> do
            addDefn $ AST.GlobalDefinition $
                G.functionDefaults
                { G.name = funcName'
                , G.parameters = parameters
                , G.returnType = retType
                , G.basicBlocks = body
                }
            msFunctions . at funcName .= Just retType
  where
    funcName' = AST.Name funcName
    bodyEither funcs vars =
        execCodegen funcs vars (toCodegen suite) >>= createBlocks
    parameters = ([G.Parameter t n [] | (t,n) <- args], False)

-- | Add global variable with given type and name to module.
addGlobalVariable
    :: (ToConstant a)
    => AST.Type -> String -> a -> LLVM ()
addGlobalVariable varType varName varExpr = do
    value <- toConstant varExpr
    addDefn $ AST.GlobalDefinition $
        G.globalVariableDefaults
        { G.name = varName'
        , G.type' = varType
        , G.initializer = Just value
        }
    msVariables . at varName .=
        Just (AST.ConstantOperand $ C.GlobalReference varType $ varName')
  where
    varName' = AST.Name varName
