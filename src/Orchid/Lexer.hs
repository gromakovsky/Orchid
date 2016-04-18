-- | Lexer is capable of parsing input file into tokens.

module Orchid.Lexer
       ( LexerState
       , lexerState
       , Lexer
       , anyL
       , firstToken
       , tokenizer
       , tokenizeInputFile
       , tokenizeInput
       , newlineL
       , nameL
       , semicolonL
       , assignL
       , passL
       , returnL
       , orL
       , andL
       , notL
       , gtL
       , ltL
       , equalL
       , leL
       , geL
       , neL
       , plusL
       , minusL
       , starL
       , slashL
       , percentL
       , doubleStarL
       , lparenL
       , rparenL
       , numberL
       , boolL
       , commaL
       , ifL
       , elseL
       , whileL
       , defL
       , colonL
       , arrowL
       , indentL
       , dedentL
       ) where

import           Control.Exception    (throwIO)
import           Control.Monad        (unless)
import           Data.List            (elemIndex, genericLength,
                                       genericReplicate)
import           Data.Text            (Text, pack)
import qualified Data.Text.IO         as TIO
import           Safe                 (headDef, headMay)
import           Text.Parsec          (alphaNum, char, choice, getState, letter,
                                       many, modifyState, oneOf, parserFail,
                                       satisfy, skipMany, try, (<|>))
import qualified Text.Parsec          as P
import           Text.Parsec.Language (emptyDef)
import           Text.Parsec.Text     (GenParser)

import           Orchid.Error         (ParserException (ParserException))
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

anyL :: Lexer
anyL =
    choice $
    map try [equalL, doubleStarL, leL, geL] ++
    [ newlineL
    , nameL
    , semicolonL
    , assignL
    , passL
    , returnL
    , orL
    , andL
    , notL
    , ltL
    , gtL
    , neL
    , plusL
    , minusL
    , starL
    , slashL
    , percentL
    , lparenL
    , rparenL
    , numberL
    , boolL
    , commaL
    , ifL
    , elseL
    , whileL
    , defL
    , colonL
    , arrowL
    , indentL
    , dedentL]

firstToken :: P.SourceName -> Text -> Either P.ParseError Token
firstToken = P.runParser anyL lexerState

tokenizer :: Parser [Token]
tokenizer = P.manyTill anyL P.eof

tokenizeInputFile :: FilePath -> IO [Token]
tokenizeInputFile fp =
    either (throwIO . ParserException) return =<<
    tokenizeInput fp <$> TIO.readFile fp

tokenizeInput :: P.SourceName -> Text -> Either P.ParseError [Token]
tokenizeInput = P.runParser tokenizer lexerState

lexerGen :: Monad m => Tok.GenTokenParser Text u m
lexerGen = Tok.makeTokenParser style
  where
    style =
        emptyDef
        { Tok.commentLine = "#"
        , Tok.identStart = letter <|> char '_'
        , Tok.identLetter = alphaNum <|> oneOf "_'$"
        , Tok.opStart = oneOf "!<>=+-*/%"
        , Tok.opLetter = Tok.opStart style
        , Tok.reservedNames = [ "pass"
                              , "return"
                              , "def"
                              , "True"
                              , "False"
                              , "if"
                              , "while"
                              , "or"
                              , "and"
                              , "not"]
        , Tok.reservedOpNames = [ "<"
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
    () <$ char '#'
    () <$ skipMany (satisfy (/= '\n'))
    () <$ char '\n'

emptyLine :: Parser ()
emptyLine = many (char ' ') >> (try oneLineComment <|> () <$ char '\n')

newlineL :: Lexer
newlineL =
    TokNewline <$
    do () <$ char '\n'
       skipMany $ try emptyLine
       LexerState {..} <- getState
       newIndent <- genericLength <$> many (char ' ')
       processNewIndent (headDef 0 lsStack) newIndent

nameL :: Lexer
nameL = verifyNoExtra >> TokName . pack <$> Tok.identifier lexerGen

semicolonL :: Lexer
semicolonL = parseSymbol ";" TokSemicolon

assignL :: Lexer
assignL = parseSymbol "=" TokAssign

passL :: Lexer
passL = parseReserved "pass" TokPass

returnL :: Lexer
returnL = parseReserved "return" TokReturn

orL :: Lexer
orL = parseReserved "or" TokOr

andL :: Lexer
andL = parseReserved "and" TokAnd

notL :: Lexer
notL = parseReserved "not" TokNot

ltL :: Lexer
ltL = parseOperator "<" TokLT

gtL :: Lexer
gtL = parseOperator ">" TokGT

equalL :: Lexer
equalL = parseOperator "==" TokEqual

leL :: Lexer
leL = parseOperator "<=" TokLE

geL :: Lexer
geL = parseOperator ">=" TokGE

neL :: Lexer
neL = parseOperator "!=" TokNE

plusL :: Lexer
plusL = parseOperator "+" TokPlus

minusL :: Lexer
minusL = parseOperator "-" TokMinus

starL :: Lexer
starL = parseOperator "*" TokStar

slashL :: Lexer
slashL = parseOperator "/" TokSlash

percentL :: Lexer
percentL = parseOperator "%" TokPercent

doubleStarL :: Lexer
doubleStarL = parseOperator "**" TokDoubleStar

lparenL :: Lexer
lparenL = parseSymbol "(" TokLParen

rparenL :: Lexer
rparenL = parseSymbol ")" TokRParen

numberL :: Lexer
numberL = verifyNoExtra >> TokNumber . fromIntegral <$> Tok.integer lexerGen

boolL :: Lexer
boolL =
    verifyNoExtra >>
    (TokBool True <$ Tok.reserved lexerGen "True" <|>
     TokBool False <$ Tok.reserved lexerGen "False")

commaL :: Lexer
commaL = parseSymbol "," TokComma

ifL :: Lexer
ifL = parseReserved "if" TokIf

elseL :: Lexer
elseL = parseReserved "else" TokElse

whileL :: Lexer
whileL = parseReserved "while" TokWhile

defL :: Lexer
defL = parseReserved "def" TokDef

colonL :: Lexer
colonL = parseSymbol ":" TokColon

arrowL :: Lexer
arrowL = parseSymbol "→" TokArrow <|> parseSymbol "->" TokArrow

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
        { lsStack = drop cnt lsStack
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

parseReserved :: String -> Token -> Lexer
parseReserved s t = verifyNoExtra >> t <$ Tok.reserved lexerGen s

parseOperator :: String -> Token -> Lexer
parseOperator s t = verifyNoExtra >> t <$ Tok.reservedOp lexerGen s

parseSymbol :: String -> Token -> Lexer
parseSymbol s t = verifyNoExtra >> t <$ Tok.symbol lexerGen s
