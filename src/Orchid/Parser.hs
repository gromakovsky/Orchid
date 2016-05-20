-- | Parser takes input file and returns AST.

module Orchid.Parser
       ( parseInputFile
       , parseInput
       ) where

import           Control.Exception (throwIO)
import           Data.Function     ((&))
import           Data.Maybe        (catMaybes, fromMaybe)
import           Data.Text         (Text)
import qualified Data.Text.IO      as TIO
import           Text.Parsec       ((<|>))
import qualified Text.Parsec       as P
import qualified Text.Parsec.Expr  as E
import           Text.Parsec.Text  (GenParser)

import           Orchid.Error      (ParserException (ParserException))
import           Orchid.Lexer      (LexerState, ampersandL, andL, arrowL,
                                    assignL, boolL, classL, colonL, commaL,
                                    dedentL, defL, dotL, doubleStarL, elseL,
                                    equalL, geL, gtL, ifL, indentL, leL,
                                    lexerState, lparenL, ltL, minusL, nameL,
                                    neL, newlineL, notL, numberL, orL, passL,
                                    percentL, plusL, privateL, publicL, returnL,
                                    rparenL, semicolonL, slashL, starL, whileL)
import qualified Orchid.Token      as Tok
import qualified Orchid.Types      as OT

type ParserState = LexerState
type Parser = GenParser ParserState

parserState :: ParserState
parserState = lexerState

parseInputFile :: FilePath -> IO OT.Input
parseInputFile fp =
    either (throwIO . ParserException) return =<<
    parseInput fp <$> TIO.readFile fp

parseInput :: P.SourceName -> Text -> Either P.ParseError OT.Input
parseInput = P.runParser parser parserState

parser :: Parser OT.Input
parser =
    OT.Input . catMaybes <$>
    (P.many (P.try (Nothing <$ newlineL) <|> (Just <$> parseStmt)) <* P.eof)

parseName :: Parser OT.Identifier
parseName = Tok.getTokName <$> nameL

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
    OT.DeclStmt <$> parseName <*> parseName <*>
    (assignL >> parseExpr)

parseExprStmt :: Parser OT.ExprStmt
parseExprStmt =
    OT.ExprStmt <$> (P.optionMaybe . P.try $ parseExpr <* assignL) <*>
    parseExpr

parseFlowStmt :: Parser OT.FlowStmt
parseFlowStmt = OT.FSReturn <$> parseReturnStmt

parseReturnStmt :: Parser OT.ReturnStmt
parseReturnStmt = do
    () <$ returnL
    OT.ReturnStmt <$> P.optionMaybe (P.try parseExpr)

parseExpr :: Parser OT.Expr
parseExpr = E.buildExpressionParser table parseEAtom
  where
    binary lexer op = E.Infix (binaryParser lexer op)
    binaryParser lexer op = OT.EBinary op <$ lexer
    unary lexer op = E.Prefix (unaryParser lexer op)
    unaryParser lexer op = OT.EUnary op <$ lexer
    n = E.AssocNone
    l = E.AssocLeft
    r = E.AssocRight
    table =
        [ [binary doubleStarL OT.BinPower r]
        , [unary starL OT.UnaryDeref, unary ampersandL OT.UnaryAddr]
        , [unary plusL OT.UnaryPlus, unary minusL OT.UnaryMinus]
        , [ binary starL OT.BinMult l
          , binary slashL OT.BinDiv l
          , binary percentL OT.BinMod l]
        , [binary plusL OT.BinPlus l, binary minusL OT.BinMinus l]
        , [ binary ltL OT.BinLT n
          , binary gtL OT.BinGT n
          , binary equalL OT.BinEQ n
          , binary leL OT.BinLE n
          , binary geL OT.BinGE n
          , binary neL OT.BinNE n]
        , [unary notL OT.UnaryNot]
        , [binary andL OT.BinAnd l]
        , [binary orL OT.BinOr l]]
    parseEAtom = OT.EAtom <$> parseAtomExpr

type Trailer = OT.AtomExpr -> OT.AtomExpr

parseAtomExpr :: Parser OT.AtomExpr
parseAtomExpr = do
    a <- OT.AEAtom <$> parseAtom
    trailers <- P.many $ P.try parseTrailer
    return $ foldl (&) a trailers
  where
    parseTrailer :: Parser Trailer
    parseTrailer =
        P.choice
            [ flip OT.AECall <$> P.between lparenL rparenL parseOptionalArgList
            , flip OT.AEAccess <$> (dotL >> parseName)]
    parseOptionalArgList =
        fromMaybe [] <$> (P.optionMaybe . P.try $ parseArgList)
    parseArgList = do
        a0 <- parseExpr
        as <- P.many . P.try $ commaL >> parseExpr
        a0 : as <$ P.optional commaL

parseAtom :: Parser OT.Atom
parseAtom =
    P.choice
        [ OT.AExpr <$> P.between lparenL rparenL parseExpr
        , P.try $ OT.ABool . Tok.getTokBool <$> boolL
        , OT.AIdentifier <$> parseName
        , OT.ANumber . Tok.getTokNumber <$> numberL]

parseCompoundStmt :: Parser OT.CompoundStmt
parseCompoundStmt =
    P.choice
        [ OT.CSIf <$> parseIf
        , OT.CSWhile <$> parseWhile
        , OT.CSFunc <$> parseFuncDef
        , OT.CSClass <$> parseClassDef]

parseIf :: Parser OT.IfStmt
parseIf =
    OT.IfStmt <$> (ifL >> parseExpr) <*> (colonL >> parseSuite) <*>
    (P.optionMaybe . P.try $ elseL >> colonL >> parseSuite)

parseWhile :: Parser OT.WhileStmt
parseWhile = OT.WhileStmt <$> (whileL >> parseExpr) <*> (colonL >> parseSuite)

parseFuncDef :: Parser OT.FuncDef
parseFuncDef =
    OT.FuncDef <$> (defL >> parseName) <*>
    (P.between lparenL rparenL parseOptionalTypedArgs) <*>
    (P.optionMaybe . P.try $ arrowL >> parseName) <*>
    (colonL >> parseSuite)
  where
    parseOptionalTypedArgs =
        fromMaybe [] <$> (P.optionMaybe . P.try $ parseTypedArgs)
    parseTypedArgs = do
        a <- parseTypedArgument
        as <- P.many . P.try $ commaL >> parseTypedArgument
        a : as <$ P.optional commaL

parseClassDef :: Parser OT.ClassDef
parseClassDef =
    OT.ClassDef <$> (classL >> parseName) <*> parseOptionalParent <*>
    (colonL >> parseClassSuite)
  where
    parseOptionalParent = P.optionMaybe $ P.between lparenL rparenL parseName

parseTypedArgument :: Parser OT.TypedArgument
parseTypedArgument =
    OT.TypedArgument <$> parseName <*> (colonL >> parseName) <*>
    (maybe False (const True) <$> P.optionMaybe starL)

parseSuite :: Parser OT.Suite
parseSuite =
    P.choice
        [ OT.Suite . replicate 1 . OT.SSimple <$> parseSimpleStmt
        , OT.Suite <$> (newlineL >> indentL >> P.many1 parseStmt <* dedentL)]

parseClassSuite :: Parser OT.ClassSuite
parseClassSuite =
    OT.ClassSuite <$>
    (newlineL >> indentL >> P.many1 parseClassStmt <* dedentL)

parseClassStmt :: Parser OT.ClassStmt
parseClassStmt = OT.ClassStmt <$> parseAccessModifier <*> parsePayload
  where
    parsePayload =
        P.choice
            [ P.try $ Left <$> parseFuncDef
            , Right <$> (parseDeclStmt <* newlineL)]

parseAccessModifier :: Parser OT.AccessModifier
parseAccessModifier =
    P.choice [P.try $ OT.AMPrivate <$ privateL, OT.AMPublic <$ publicL]
