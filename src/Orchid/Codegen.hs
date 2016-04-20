{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | Primitive functions to work with LLVM code generation.

module Orchid.Codegen
       (
         -- Module level
         LLVM
       , execLLVM
       , emptyModule
       , addDefn
       , addFuncDef

         -- Helpers
       , throwCodegenError

         -- Types
       , int64
       , bool

         -- Codegen
       , Codegen
       , createBlocks
       , execCodegen

         -- Symbol table
       , assign
       , getVar

         -- References
       , local

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

         -- Effects
       , call
       , alloca
       , store
       , load

         -- Control Flow
       , ret

         -- Reexports
       , Type (..)
       , void
       , AST.Name (..)
       ) where

import           Control.Lens                       (makeLenses, makeLensesFor,
                                                     use, view, (%=), (+=),
                                                     (.=), (.~), (<>~))
import           Control.Monad.Except               (ExceptT,
                                                     MonadError (throwError),
                                                     runExceptT)
import           Control.Monad.State                (MonadState, State,
                                                     execState, runState)
import           Data.Function                      (on, (&))
import           Data.Int                           (Int64)
import           Data.List                          (sortBy)
import qualified Data.Map                           as M
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
-- Module Level
-------------------------------------------------------------------------------

$(makeLensesFor [("moduleDefinitions", "mDefinitions")] ''AST.Module)

newtype LLVM a = LLVM
    { getLLVM :: ExceptT CodegenException (State AST.Module) a
    } deriving (Functor,Applicative,Monad,MonadState AST.Module,MonadError CodegenException)

execLLVM :: AST.Module -> LLVM a -> Either CodegenException AST.Module
execLLVM m = f . flip runState m . runExceptT . getLLVM
  where
    f (Left e,_) = Left e
    f (_,s') = Right s'

emptyModule :: String -> AST.Module
emptyModule label =
    AST.defaultModule
    { AST.moduleName = label
    }

addDefn :: AST.Definition -> LLVM ()
addDefn d = mDefinitions %= (++ [d])

addFuncDef :: AST.Type
           -> String
           -> [(AST.Type, AST.Name)]
           -> [G.BasicBlock]
           -> LLVM ()
addFuncDef retType funcName args body =
    addDefn $
    AST.GlobalDefinition $
    G.functionDefaults
    { G.name = AST.Name funcName
    , G.parameters = ([G.Parameter t n [] | (t, n) <- args], False)
    , G.returnType = retType
    , G.basicBlocks = body
    }

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

throwCodegenError
    :: MonadError CodegenException m
    => Text -> m a
throwCodegenError = throwError . CodegenException

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

int64, bool :: Type
int64 = i64
bool = i1

-------------------------------------------------------------------------------
-- Names
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- Codegen State
-------------------------------------------------------------------------------

data BlockState = BlockState
    { _bsIdx   :: Int                            -- Block index
    , _bsStack :: [AST.Named AST.Instruction]            -- Stack of instructions
    , _bsTerm  :: Maybe (AST.Named AST.Terminator)       -- Block terminator
    } deriving (Show)

$(makeLenses ''BlockState)

type SymbolTable = [(String, AST.Operand)]

data CodegenState = CodegenState
    { _csCurrentBlock :: AST.Name               -- Name of the active block to append to
    , _csBlocks       :: M.Map AST.Name BlockState  -- Blocks for function
    , _csSymtab       :: SymbolTable            -- Function scope symbol table
    , _csBlockCount   :: Int                    -- Count of basic blocks
    , _csCount        :: Word                   -- Count of unnamed instructions
    , _csNames        :: Names                  -- Name Supply
    } deriving (Show)

$(makeLenses ''CodegenState)

-------------------------------------------------------------------------------
-- Codegen Operations
-------------------------------------------------------------------------------

newtype Codegen a = Codegen
    { getCodegen :: ExceptT CodegenException (State CodegenState) a
    } deriving (Functor,Applicative,Monad,MonadState CodegenState,MonadError CodegenException)

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

entryBlockName :: String
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: CodegenState
emptyCodegen =
    execState (runExceptT . getCodegen $ addBlock entryBlockName) $
    CodegenState
    { _csCurrentBlock = (AST.Name entryBlockName)
    , _csBlocks = M.empty
    , _csSymtab = []
    , _csBlockCount = 0
    , _csCount = 0
    , _csNames = M.empty
    }

execCodegen :: Codegen a -> Either CodegenException CodegenState
execCodegen = f . flip runState emptyCodegen . runExceptT . getCodegen
  where
    f (Left e,_) = Left e
    f (_,s') = Right s'

fresh :: Codegen Word
fresh = do
    csCount += 1
    use csCount

instr :: AST.Instruction -> Codegen AST.Operand
instr ins = do
    n <- fresh
    let ref = AST.UnName n
    currentBlock <- getCurrentBlock
    setCurrentBlock $ currentBlock & bsStack <>~ [ref AST.:= ins]
    -- FIXME: pass type somehow
    return $ local int64 ref

terminator :: AST.Named AST.Terminator -> Codegen (AST.Named AST.Terminator)
terminator trm =
    trm <$
    do currentBlock <- getCurrentBlock
       setCurrentBlock $ currentBlock & bsTerm .~ Just trm

-------------------------------------------------------------------------------
-- Block Stack
-------------------------------------------------------------------------------

addBlock :: String -> Codegen AST.Name
addBlock blockName = do
    ix <- use csBlockCount
    names <- use csNames
    let new = emptyBlock ix
        (qname,supply) = uniqueName blockName names
        astName = AST.Name qname
    csBlocks %= M.insert astName new
    csBlockCount += 1
    csNames .= supply
    return astName

-- setBlock :: Name -> Codegen Name
-- setBlock bname = do
--   modify $ \s -> s { currentBlock = bname }
--   return bname

getCurrentBlock :: Codegen BlockState
getCurrentBlock = do
    v <- M.lookup <$> use csCurrentBlock <*> use csBlocks
    maybe
        (throwCodegenError . formatSingle' "no such block: {}" =<<
         use csCurrentBlock)
        return
        v

setCurrentBlock :: BlockState -> Codegen ()
setCurrentBlock bs = do
    name <- use csCurrentBlock
    csBlocks %= M.insert name bs

-------------------------------------------------------------------------------
-- Symbol Table
-------------------------------------------------------------------------------

assign :: String -> AST.Operand -> Codegen ()
assign var x = csSymtab %= ((var, x) :)

getVar :: String -> Codegen AST.Operand
getVar var = do
    maybe reportError return =<< lookup var <$> use csSymtab
  where
    reportError =
        throwCodegenError $
        formatSingle' "local variable is not is scope: {}" var

-------------------------------------------------------------------------------
-- References
-------------------------------------------------------------------------------

local :: Type -> AST.Name -> AST.Operand
local = AST.LocalReference

-------------------------------------------------------------------------------
-- Unary operations
-------------------------------------------------------------------------------

neg :: AST.Operand -> Codegen AST.Operand
neg = sub $ constInt64 0

not :: AST.Operand -> Codegen AST.Operand
not = xor $ constBool True

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

constInt64 :: Int64 -> AST.Operand
constInt64 = AST.ConstantOperand . C.Int 64 . toInteger

constBool :: Bool -> AST.Operand
constBool = AST.ConstantOperand . C.Int 1 . boolToInteger
  where
    boolToInteger False = 0
    boolToInteger True = 1

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
-- br :: Name -> Codegen (Named Terminator)
-- br val = terminator $ Do $ Br val []

-- cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
-- cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

ret :: Maybe AST.Operand -> Codegen (AST.Named AST.Terminator)
ret = terminator . AST.Do . flip AST.Ret []
