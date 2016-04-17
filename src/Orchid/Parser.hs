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

import           Orchid.Lexer     (LexerState, assignL, boolL, commaL,
                                   lexerState, lparenL, nameL, newlineL,
                                   numberL, passL, returnL, rparenL, semicolonL)
import qualified Orchid.Token     as Tok
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
    OT.SCompound <$> parseCompoundStmt

parseSimpleStmt :: Parser OT.SimpleStmt
parseSimpleStmt =
    OT.SimpleStmt <$>
    do s0 <- parseSmallStmt
       ss <- P.many (P.try $ semicolonL >> parseSmallStmt)
       () <$ P.optional semicolonL
       () <$ newlineL
       return (s0:ss)

parseSmallStmt :: Parser OT.SmallStmt
parseSmallStmt =
    P.choice
        [ P.try $ OT.SSDecl <$> parseDeclStmt
        , P.try $ OT.SSExpr <$> parseExprStmt
        , OT.SSPass <$ passL
        , OT.SSFlow <$> parseFlowStmt]

parseDeclStmt :: Parser OT.DeclStmt
parseDeclStmt =
    OT.DeclStmt <$> (Tok.getTokName <$> nameL) <*> (Tok.getTokName <$> nameL) <*>
    (assignL >> parseExpr)

parseExprStmt :: Parser OT.ExprStmt
parseExprStmt =
    OT.ExprStmt <$>
    (P.optionMaybe . P.try $ Tok.getTokName <$> nameL <* assignL) <*>
    parseExpr

parseFlowStmt :: Parser OT.FlowStmt
parseFlowStmt = OT.FSReturn <$> parseReturnStmt

parseReturnStmt :: Parser OT.ReturnStmt
parseReturnStmt = do
    () <$ returnL
    OT.ReturnStmt <$> P.optionMaybe (P.try parseExpr)

-- TODO
parseExpr :: Parser OT.Expr
parseExpr = parseEAtom
  where
    parseEAtom = OT.EAtom <$> parseAtomExpr

parseAtomExpr :: Parser OT.AtomExpr
parseAtomExpr = do
    a <- parseAtom
    maybe (OT.AEAtom a) (OT.AECall a) <$> P.optionMaybe (P.try parseTrailer)
  where
    parseTrailer = P.between lparenL rparenL parseArglist
    parseArglist = do
        a0 <- parseExpr
        as <- P.many . P.try $ commaL >> parseExpr
        a0 : as <$ P.optional commaL

parseAtom :: Parser OT.Atom
parseAtom =
    P.choice
        [ OT.AExpr <$> P.between lparenL rparenL parseExpr
        , P.try $ OT.ABool . Tok.getTokBool <$> boolL
        , OT.AIdentifier . Tok.getTokName <$> nameL
        , OT.ANumber . Tok.getTokNumber <$> numberL]

parseCompoundStmt :: Parser OT.CompoundStmt
parseCompoundStmt = fail "undefined"
