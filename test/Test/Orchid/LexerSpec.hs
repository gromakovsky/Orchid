-- | Lexer specification.

module Test.Orchid.LexerSpec
       ( spec
       ) where

import           Data.Either           (isLeft)
import           Data.Monoid           ((<>))
import           Data.Text             (Text, intercalate)
import           Test.Hspec            (Spec, describe, it, shouldBe,
                                        shouldSatisfy)
import           Test.Hspec.QuickCheck (prop)

import           Orchid.Lexer          (firstToken, tokenizeInput)
import           Orchid.Token          (Token (..), tokenRepr)

import           Test.Orchid.Arbitrary (PrintableToken (..))

spec :: Spec
spec =
    describe "Lexer" $ do
        describe "firstToken" $ do
            it "parses first token of the input" $ do
                fToken "or" `shouldBe` Right TokOr
                fToken "+" `shouldBe` Right TokPlus
                fToken "def" `shouldBe` Right TokDef
                fToken "patak" `shouldBe` Right (TokName "patak")
                fToken "False a b \r\r\r\t" `shouldBe` Right (TokBool False)
                fToken "<" `shouldBe` Right TokLT
                fToken "<=" `shouldBe` Right TokLE
                fToken "" `shouldSatisfy` isLeft
                fToken "#ks;dfklsf\n" `shouldSatisfy` isLeft
                fToken "$$$$$$$$" `shouldSatisfy` isLeft
            prop
                "given representation of printable token, space and arbitrary stuff after it, returns that token"
                firstTokenOnReprOfPrintable
        describe "tokenizeInput" $ do
            it "parses input file and yields stream of tokens" $ do
                tokenize "a or b" `shouldBe` Right [ TokName "a"
                                                   , TokOr
                                                   , TokName "b"]
                tokenize "def f():\n  #example function\n  pass" `shouldBe`
                    Right [ TokDef
                          , TokName "f"
                          , TokLParen
                          , TokRParen
                          , TokColon
                          , TokNewline
                          , TokIndent
                          , TokPass ]
            prop
                "given sequence of printable tokens (except \\n) delimited by spaces, returns those tokens"
                tokenizeInputOnPrintableSequenceExceptNewline
  where
    sourceName = "test-suite"
    fToken = firstToken sourceName
    tokenize = tokenizeInput sourceName

firstTokenOnReprOfPrintable :: PrintableToken -> Text -> Bool
firstTokenOnReprOfPrintable (PrintableToken t) stuff =
    either (const False) (== t) $
    firstToken "firstTokenOnReprOfPrintable" (tokenRepr t <> " " <> stuff)

tokenizeInputOnPrintableSequenceExceptNewline :: [PrintableToken] -> Bool
tokenizeInputOnPrintableSequenceExceptNewline pTokens =
    either (const False) (== tokens) $
    tokenizeInput
        "tokenizeInputOnPrintableSequenceExceptNewline"
        (intercalate " " $ map tokenRepr tokens)
  where
    tokens = filter (/= TokNewline) . map getPrintableToken $ pTokens
