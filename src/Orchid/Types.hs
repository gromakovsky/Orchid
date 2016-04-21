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
       , ClassDef (..)
       , TypedArgument (..)
       , Suite (..)
       , ClassSuite (..)
       , ClassStmt (..)
       , AccessModifier (..)
       ) where

import           Data.Int  (Int64)
import           Data.Text (Text)

type Identifier = Text
type Number = Int64

-- | Representation of input file passed to compiler.
-- Input file contains list of statements.
-- file_input → (NEWLINE | stmt)*
newtype Input = Input
    { getInput :: [Stmt]
    } deriving (Show, Eq, Monoid)

-- | Statement is either simple statement or compound statement.
-- stmt → simple_stmt | compound_stmt
data Stmt
    = SSimple !SimpleStmt
    | SCompound !CompoundStmt
    deriving (Show, Eq)

-- | Simple statement is a list of small statements.
-- simple_stmt → small_stmt (';' small_stmt)* [';'] NEWLINE
data SimpleStmt =
    SimpleStmt ![SmallStmt]
    deriving (Show, Eq)

-- | Small statement is either variable declaration or expression
-- statement or pass statement or flow statement.
-- small_stmt → decl_stmt | expr_stmt | pass_stmt | flow_stmt
data SmallStmt
    = SSDecl !DeclStmt
    | SSExpr !ExprStmt
    | SSPass
    | SSFlow !FlowStmt
    deriving (Show, Eq)

-- | Declaration statement declares variable. It contains variable
-- type, variable name and initial value.
-- decl_stmt → NAME NAME '=' expr
data DeclStmt = DeclStmt
    { dsType :: !Identifier
    , dsVar  :: !Identifier
    , dsExpr :: !Expr
    } deriving (Show, Eq)

-- | Expression statement executes expression and optionally assigns
-- result to variable.
-- expr_stmt → [NAME '='] expr
data ExprStmt = ExprStmt
    { esVar  :: !(Maybe Identifier)
    , esExpr :: !Expr
    } deriving (Show, Eq)

-- | Flow statement represents control flow operators. Only `return`
-- is supported at this point.
-- flow_stmt → return_stmt
data FlowStmt =
    FSReturn !ReturnStmt
    deriving (Show, Eq)

-- | Return statement contains optional return value.
-- return_stmt → 'return' [expr]
data ReturnStmt =
    ReturnStmt !(Maybe Expr)
    deriving (Show, Eq)

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
    deriving (Show, Eq)

-- | Unary operation is either unary plus or unary minus or boolean `not`.
data UnaryOp
    = UnaryPlus
    | UnaryMinus
    | UnaryNot
    deriving (Show, Eq)

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
    deriving (Show, Eq)

-- | Atom expression is either Atom or function call.
-- atom_expr → atom [trailer]
-- trailer → '(' [arglist] ')'
-- arglist → argument (',' argument)* [',']
-- argument → expr
data AtomExpr
    = AEAtom !Atom
    | AECall !Atom
             ![Expr]
    deriving (Show, Eq)

-- | Atom is either an expression or identifier or number or boolean
-- constant.
-- atom → '(' expr ')' | NAME | NUMBER | 'True' | 'False'
data Atom
    = AExpr !Expr
    | AIdentifier !Identifier
    | ANumber !Number
    | ABool !Bool
    deriving (Show, Eq)

-- | Compound statement is either if statement or while statement or
-- function definition.
-- compound_stmt → if_stmt | while_stmt | funcdef | classdef
data CompoundStmt
    = CSIf !IfStmt
    | CSWhile !WhileStmt
    | CSFunc !FuncDef
    | CSClass !ClassDef
    deriving (Show, Eq)

-- | If statement contains condition expression, suite to execute in
-- True case and optional suite to execute in False case.
-- if_stmt → 'if' expr ':' suite ['else' ':' suite]
data IfStmt =
    IfStmt !Expr
           !Suite
           !(Maybe Suite)
    deriving (Show, Eq)

-- | While statement contains condition expression and body suite.
-- while_stmt → 'while' expr ':' suite
data WhileStmt =
    WhileStmt !Expr
              !Suite
    deriving (Show, Eq)

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
    } deriving (Show, Eq)

-- | Class definition contains class name, optional parent in
-- inheritence hierarchy and body suite.
-- classdef → 'class' NAME ['(' NAME ')'] ':' class_suite
data ClassDef = ClassDef
    { clsName   :: !Identifier
    , clsParent :: !(Maybe Identifier)
    , clsBody   :: !ClassSuite
    } deriving (Show,Eq)

-- | Type argument consists of name and type.
-- typedarg → NAME ':' NAME
data TypedArgument = TypedArgument
    { taName :: !Identifier
    , taType :: !Identifier
    } deriving (Show, Eq)

-- | Suite is basically a list of statements.
-- suite → simple_stmt | NEWLINE INDENT stmt+ DEDENT
newtype Suite = Suite
    { getSuite :: [Stmt]
    } deriving (Show, Eq)

-- | Class suite is basically a list of class statements.
-- class_suite → NEWLINE INDENT class_stmt+ DEDENT
newtype ClassSuite = ClassSuite
    { getClassSuite :: [ClassStmt]
    } deriving (Show, Eq)

-- | Class statement is either function defition or variable
-- declaration prefixed with access modifier
-- class_stmt → access_modifier (funcdef | decl_stmt NEWLINE)
data ClassStmt = ClassStmt
    { csAccess  :: !AccessModifier
    , csPayload :: !(Either FuncDef DeclStmt)
    } deriving (Show, Eq)

-- | Access modifier determines how has access to function/variable in
-- class. It is either `private` or `public`.
-- access_modifier → 'private' | 'public'
data AccessModifier
    = AMPrivate
    | AMPublic
    deriving (Show,Eq)
