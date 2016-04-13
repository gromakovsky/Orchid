-- | Tokens are result of input processing by lexer.

module Orchid.Token
       ( Token (..)
       ) where

import           Orchid.Types (Identifier, Number)

data Token
    = TokNewline
    | TokName Identifier
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
    | TokNumber Number
    | TokBool Bool
    | TokComma
    | TokIf
    | TokWhile
    | TokDef
    | TokColon
    | TokArrow
    | TokIndent
    | TokDedent
    deriving (Show)
