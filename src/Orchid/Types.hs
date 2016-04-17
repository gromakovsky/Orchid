-- | Basic types used in compiler.

module Orchid.Types
       ( Identifier
       , Number
       , Input (..)
       , Stmt (..)
       , SimpleStmt (..)
       , SmallStmt (..)
       , DeclStmt (..)
       , ExprStmt (..)
       , FlowStmt (..)
       , ReturnStmt (..)
       , Expr (..)
       , UnaryOp (..)
       , BinaryOp (..)
       , AtomExpr (..)
       , Atom (..)
       , CompoundStmt (..)
       , IfStmt (..)
       , WhileStmt (..)
       , FuncDef (..)
       , TypedArgument (..)
       , Suite (..)
       ) where

import           Data.Int  (Int32)
import           Data.Text (Text)

type Identifier = Text
type Number = Int32

-- | Representation of input file passed to compiler.
-- Input file contains list of statements.
-- file_input → (NEWLINE | stmt)*
newtype Input = Input
    { getInput :: [Stmt]
    } deriving (Show)

-- | Statement is either simple statement or compound statement.
-- stmt → simple_stmt | compound_stmt
data Stmt
    = SSimple !SimpleStmt
    | SCompound !CompoundStmt
    deriving (Show)

-- | Simple statement is a list of small statements.
-- simple_stmt → small_stmt (';' small_stmt)* [';'] NEWLINE
data SimpleStmt =
    SimpleStmt ![SmallStmt]
    deriving (Show)

-- | Small statement is either variable declaration or expression
-- statement or pass statement or flow statement.
-- small_stmt → decl_stmt | expr_stmt | pass_stmt | flow_stmt
data SmallStmt
    = SSDecl !DeclStmt
    | SSExpr !ExprStmt
    | SSPass
    | SSFlow !FlowStmt
    deriving (Show)

-- | Declaration statement declares variable. It contains variable
-- type, variable name and initial value.
-- decl_stmt → NAME NAME '=' expr
data DeclStmt = DeclStmt
    { dsType :: !Identifier
    , dsVar  :: !Identifier
    , dsExpr :: !Expr
    } deriving (Show)

-- | Expression statement executes expression and optionally assigns
-- result to variable.
-- expr_stmt → [NAME '='] expr
data ExprStmt = ExprStmt
    { esVar  :: !(Maybe Identifier)
    , esExpr :: !Expr
    } deriving (Show)

-- | Flow statement represents control flow operators. Only `return`
-- is supported at this point.
-- flow_stmt → return_stmt
data FlowStmt =
    FSReturn !ReturnStmt
    deriving (Show)

-- | Return statement contains optional return value.
-- return_stmt → 'return' [expr]
data ReturnStmt =
    ReturnStmt !(Maybe Expr)
    deriving (Show)

-- | Expression is either unary operation applied to expression or
-- binary operation applied to two expressions or atom expression.
-- expr → and_test ('or' and_test)*
-- and_test → not_test ('and' not_test)*
-- not_test → 'not' not_test | comparison
-- comparison → arith_expr (comp_op arith_expr)*
-- comp_op → '<' | '>' | '==' | '<=' | '>=' | '!='
-- arith_expr → term (('+' | '-') term)*
-- term → factor (('*', '/', '%') factor)*
-- factor → ('+', '-') factor | power
-- power → atom_expr ['**' factor]
data Expr
    = EUnary !UnaryOp
             !Expr
    | EBinary !BinaryOp
              !Expr
              !Expr
    | EAtom !AtomExpr
    deriving (Show)

-- | Unary operation is either unary plus or unary minus or boolean `not`.
data UnaryOp
    = UnaryPlus
    | UnaryMinus
    | UnaryNot
    deriving (Show)

-- | Binary operation is one of `or`, `and`, `<`, `>`, `==`, `<=`,
-- `>=`, `!=`, "+", "-", "*", "/", "%", "**".
data BinaryOp
    = BinOr
    | BinAnd
    | BinLT
    | BinGT
    | BinEQ
    | BinLE
    | BinGE
    | BinNE
    | BinPlus
    | BinMinus
    | BinMult
    | BinDiv
    | BinMod
    | BinPower
    deriving (Show)

-- | Atom expression is either Atom or function call.
-- atom_expr → atom [trailer]
-- trailer → '(' [arglist] ')'
-- arglist → argument (',' argument)* [',']
-- argument → expr
data AtomExpr
    = AEAtom !Atom
    | AECall !Atom
             ![Expr]
    deriving (Show)

-- | Atom is either an expression or identifier or number or boolean
-- constant.
-- atom → '(' expr ')' | NAME | NUMBER | 'True' | 'False'
data Atom
    = AExpr !Expr
    | AIdentifier !Identifier
    | ANumber !Number
    | ABool !Bool
    deriving (Show)

-- | Compound statement is either if statement or while statement or
-- function definition.
-- compound_stmt → if_stmt | while_stmt | funcdef
data CompoundStmt
    = CSIf !IfStmt
    | CSWhile !WhileStmt
    | CSFunc !FuncDef
    deriving (Show)

-- | If statement contains condition expression, suite to execute in
-- True case and optional suite to execute in False case.
-- if_stmt → 'if' expr ':' suite ['else' ':' suite]
data IfStmt =
    IfStmt !Expr
           !Suite
           !(Maybe Suite)
    deriving (Show)

-- | While statement contains condition expression and body suite.
-- while_stmt → 'while' expr ':' suite
data WhileStmt =
    WhileStmt !Expr
              !Suite
    deriving (Show)

-- | Function definition contains function name, list of typed
-- arguments, optional return value and body suite.
-- funcdef → 'def' NAME parameters ['→' NAME] ':' suite
-- parameters → '(' [typedarglist] ')'
-- typedarglist → typedarg (',' typedarg)* [',']
data FuncDef = FuncDef
    { funcName :: !Identifier
    , funcArgs :: ![TypedArgument]
    , funcRet  :: !(Maybe Identifier)
    , funcBody :: !Suite
    } deriving (Show)

-- | Type argument consists of name and type.
-- typedarg → NAME ':' NAME
data TypedArgument = TypedArgument
    { taName :: !Identifier
    , taType :: !Identifier
    } deriving (Show)

-- | Suite is basically a list of statements.
-- suite → simple_stmt | NEWLINE INDENT stmt+ DEDENT
newtype Suite = Suite
    { getSuite :: [Stmt]
    } deriving (Show)
