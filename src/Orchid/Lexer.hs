-- | Lexer is capable of parsing input file into tokens.

module Orchid.Lexer
       ( LexerState
       , lexerState
       , Lexer
       , newlineL
       , nameL
       , semicolonL
       , assignL
       , passL
       , returnL
       , lparenL
       , rparenL
       , numberL
       , boolL
       , commaL
       , ifL
       , whileL
       , defL
       , colonL
       , indentL
       , dedentL
       ) where

import           Control.Monad        (unless)
import           Data.List            (elemIndex, genericLength,
                                       genericReplicate)
import           Data.Text            (Text, pack)
import           Safe                 (headDef, headMay)
import           Text.Parsec          (alphaNum, char, getState, letter, many,
                                       modifyState, oneOf, parserFail, satisfy,
                                       skipMany, try, (<|>))
import           Text.Parsec.Language (emptyDef)
import           Text.Parsec.Text     (GenParser)

import qualified Orchid.ParsecToken   as Tok
import           Orchid.Token         (Token (..))

data LexerState = LexerState
    { lsStack       :: ![Word]
    , lsExtraTokens :: ![Token]
    } deriving (Show)

lexerState :: LexerState
lexerState =
    LexerState
    { lsStack = [0]
    , lsExtraTokens = []
    }

type Parser = GenParser LexerState
type Lexer = Parser Token

lexerGen :: Monad m => Tok.GenTokenParser Text u m
lexerGen = Tok.makeTokenParser style
  where
    style =
        emptyDef
        { Tok.commentLine = "#"
        , Tok.identStart = letter <|> char '_'
        , Tok.identLetter = alphaNum <|> oneOf "_'$"
        , Tok.opStart = oneOf "|&!<>=+-*/%"
        , Tok.opLetter = Tok.opStart style
        , Tok.reservedNames = ["pass", "return", "def", "True", "False", "if"]
        , Tok.reservedOpNames = [ "||"
                                , "&&"
                                , "!"
                                , "<"
                                , ">"
                                , "=="
                                , "<="
                                , ">="
                                , "!="
                                , "+"
                                , "-"
                                , "*"
                                , "/"
                                , "%"
                                , "**"]
        , Tok.caseSensitive = True
        }

oneLineComment :: Parser ()
oneLineComment = do
    () <$ try (char '#')
    () <$ skipMany (satisfy (/= '\n'))
    () <$ char '\n'

newlineL :: Lexer
newlineL =
    TokNewline <$
    do () <$ char '\n'
       skipMany oneLineComment
       LexerState {..} <- getState
       newIndent <- genericLength <$> many (char ' ')
       processNewIndent (headDef 0 lsStack) newIndent

nameL :: Lexer
nameL = verifyNoExtra >> TokName . pack <$> Tok.identifier lexerGen

semicolonL :: Lexer
semicolonL = verifyNoExtra >> TokSemicolon <$ Tok.semi lexerGen

assignL :: Lexer
assignL = verifyNoExtra >> TokAssign <$ Tok.symbol lexerGen "="

passL :: Lexer
passL = verifyNoExtra >> TokPass <$ Tok.symbol lexerGen "pass"

returnL :: Lexer
returnL = verifyNoExtra >> TokReturn <$ Tok.symbol lexerGen "return"

lparenL :: Lexer
lparenL = verifyNoExtra >> TokLParen <$ Tok.symbol lexerGen "("

rparenL :: Lexer
rparenL = verifyNoExtra >> TokRParen <$ Tok.symbol lexerGen ")"

numberL :: Lexer
numberL = TokNumber . fromIntegral <$> (verifyNoExtra >> Tok.integer lexerGen)

boolL :: Lexer
boolL =
    verifyNoExtra >>
    (TokBool True <$ Tok.reserved lexerGen "True" <|>
     TokBool False <$ Tok.reserved lexerGen "False")

commaL :: Lexer
commaL = TokComma <$ Tok.comma lexerGen

ifL :: Lexer
ifL = TokIf <$ Tok.reserved lexerGen "if"

whileL :: Lexer
whileL = TokWhile <$ Tok.reserved lexerGen "while"

defL :: Lexer
defL = TokDef <$ Tok.reserved lexerGen "def"

colonL :: Lexer
colonL = TokColon <$ Tok.colon lexerGen

indentL :: Lexer
indentL = readExtraToken TokIndent

dedentL :: Lexer
dedentL = readExtraToken TokDedent

processNewIndent :: Word -> Word -> Parser ()
processNewIndent topIndent newIndent
  | topIndent == newIndent = return ()
  | topIndent > newIndent = do
      LexerState{..} <- getState
      maybe reportInvalidIndent (modifyState . onDedent . fromIntegral) $
          newIndent `elemIndex` lsStack
  | otherwise = modifyState $ appendIndent newIndent
  where
    reportInvalidIndent = parserFail "invalid indentation"
    onDedent cnt LexerState{..} =
        LexerState
        { lsStack = drop (cnt + 1) lsStack
        , lsExtraTokens = genericReplicate cnt TokDedent ++ lsExtraTokens
        }
    appendIndent i LexerState{..} =
        LexerState
        { lsStack = i : lsStack
        , lsExtraTokens = TokIndent : lsExtraTokens
        }

readExtraToken :: Token -> Lexer
readExtraToken tok = do
    top <- headMay . lsExtraTokens <$> getState
    if top == Just tok
        then tok <$ modifyState dropExtraTop
        else parserFail $ mconcat [show tok, " was expected, but wasn't found"]
  where
    dropExtraTop LexerState{..} =
        LexerState
        { lsExtraTokens = tail lsExtraTokens
        , ..
        }

verifyNoExtra :: Parser ()
verifyNoExtra = do
    extraEmpty <- null . lsExtraTokens <$> getState
    unless extraEmpty $ parserFail "Unexpected indent/dedent"
