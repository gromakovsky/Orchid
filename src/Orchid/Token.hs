-- | Tokens are result of input processing by lexer.

module Orchid.Token
       ( Token (..)
       , tokenRepr
       ) where

import           Data.Text    (Text, pack)

import           Orchid.Types (Identifier, Number)

data Token
    = TokNewline
    | TokName { getTokName :: !Identifier}
    | TokSemicolon
    | TokAssign
    | TokPass
    | TokReturn
    | TokOr
    | TokAnd
    | TokNot
    | TokLT
    | TokGT
    | TokEqual
    | TokLE
    | TokGE
    | TokNE
    | TokPlus
    | TokMinus
    | TokStar
    | TokSlash
    | TokPercent
    | TokDoubleStar
    | TokLParen
    | TokRParen
    | TokNumber { getTokNumber :: !Number}
    | TokBool { getTokBool :: !Bool}
    | TokComma
    | TokIf
    | TokElse
    | TokWhile
    | TokDef
    | TokClass
    | TokColon
    | TokArrow
    | TokIndent
    | TokDedent
    | TokPublic
    | TokPrivate
    deriving (Show,Eq)

tokenRepr :: Token -> Text
tokenRepr TokNewline = "\n"
tokenRepr (TokName n) = n
tokenRepr TokSemicolon = ";"
tokenRepr TokAssign = "="
tokenRepr TokPass = "pass"
tokenRepr TokReturn = "return"
tokenRepr TokOr = "or"
tokenRepr TokAnd = "and"
tokenRepr TokNot = "not"
tokenRepr TokLT = "<"
tokenRepr TokGT = ">"
tokenRepr TokEqual = "=="
tokenRepr TokLE = "<="
tokenRepr TokGE = ">="
tokenRepr TokNE = "!="
tokenRepr TokPlus = "+"
tokenRepr TokMinus = "-"
tokenRepr TokStar = "*"
tokenRepr TokSlash = "/"
tokenRepr TokPercent = "%"
tokenRepr TokDoubleStar = "**"
tokenRepr TokLParen = "("
tokenRepr TokRParen = ")"
tokenRepr (TokNumber n) = pack $ show n
tokenRepr (TokBool b) = pack $ show b
tokenRepr TokComma = ","
tokenRepr TokIf = "if"
tokenRepr TokElse = "else"
tokenRepr TokWhile = "while"
tokenRepr TokDef = "def"
tokenRepr TokClass = "class"
tokenRepr TokColon = ":"
tokenRepr TokArrow = "→"
tokenRepr TokIndent = "<INDENT>"
tokenRepr TokDedent = "<DEDENT>"
tokenRepr TokPublic = "public"
tokenRepr TokPrivate = "private"
