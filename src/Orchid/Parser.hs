-- | Parser takes input file and returns AST.

module Orchid.Parser
       ( parseInputFile
       , parseInput
       ) where

import           Data.Maybe       (catMaybes)
import           Data.Text        (Text)
import qualified Data.Text.IO     as TIO
import           Text.Parsec      ((<|>))
import qualified Text.Parsec      as P
import           Text.Parsec.Text (GenParser)

import           Orchid.Lexer     (LexerState, lexerState, newlineL, passL,
                                   semicolonL)
import qualified Orchid.Types     as OT

type ParserState = LexerState
type Parser = GenParser ParserState

parserState :: ParserState
parserState = lexerState

parseInputFile :: FilePath -> IO (Either P.ParseError OT.Input)
parseInputFile fp = parseInput fp <$> TIO.readFile fp

parseInput :: P.SourceName -> Text -> Either P.ParseError OT.Input
parseInput = P.runParser parser parserState

parser :: Parser OT.Input
parser =
    OT.Input . catMaybes <$>
    (P.many (P.try (Nothing <$ newlineL) <|> (Just <$> parseStmt)) <* P.eof)

parseStmt :: Parser OT.Stmt
parseStmt =
    P.try (OT.SSimple <$> parseSimpleStmt) <|>
    (OT.SCompound <$> parseCompoundStmt)

parseSimpleStmt :: Parser OT.SimpleStmt
parseSimpleStmt =
    OT.SimpleStmt <$>
    do s0 <- parseSmallStmt
       ss <- P.many (P.try $ semicolonL >> parseSmallStmt)
       () <$ P.optional semicolonL
       () <$ newlineL
       return (s0:ss)

parseSmallStmt :: Parser OT.SmallStmt
parseSmallStmt = OT.SSPass <$ passL

parseCompoundStmt :: Parser OT.CompoundStmt
parseCompoundStmt = fail "undefined"
