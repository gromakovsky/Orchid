-- | Arbitrary instances for various Orchid-related types.

module Orchid.Arbitrary
       ( PrintableToken (..)
       ) where

import           Data.Text           (Text, cons, pack)
import           Test.QuickCheck     (Arbitrary (arbitrary))
import           Test.QuickCheck.Gen (Gen, elements, listOf)

import           Orchid.Token        (Token (..))

newtype PrintableToken = PrintableToken
    { getPrintableToken :: Token
    } deriving (Show)

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary

genIdentifierStartSymbol :: Gen Char
genIdentifierStartSymbol = elements . mconcat $ [['a' .. 'z'], ['A' .. 'Z'], ['_']]

genIdentifierSymbol :: Gen Char
genIdentifierSymbol = elements . mconcat $ [['a' .. 'z'], ['A' .. 'Z'], ['_'], ['0' .. '9']]

instance Arbitrary PrintableToken where
    arbitrary = do
        name <-
            cons <$> genIdentifierStartSymbol <*>
            (pack <$> listOf genIdentifierSymbol)
        -- TODO: add all
        PrintableToken <$>
            elements
                [ TokNewline
                , TokName name
                , TokSemicolon
                , TokAssign
                , TokPass
                , TokReturn
                , TokOr]
