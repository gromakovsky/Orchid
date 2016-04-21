{-# LANGUAGE ExistentialQuantification #-}

-- | Errors hierarchy in Orchid compiler.

module Orchid.Error
       ( SomeOrchidException (..)
       , ParserException (..)
       , CodegenException (..)
       , LowLevelException (..)
       , FatalError (..)
       ) where

import           Control.Exception (Exception (fromException, toException, displayException),
                                    SomeException)
import           Data.Text         (Text, unpack)
import           Data.Typeable     (Typeable, cast)
import           Text.Parsec.Error (ParseError)

-- | Root exception type for all the exceptions in Orchid compiler.
data SomeOrchidException =
    forall e. Exception e => SomeOrchidException e
    deriving (Typeable)

instance Show SomeOrchidException where
    show (SomeOrchidException e) = show e

instance Exception SomeOrchidException

orchidExceptionToException :: Exception e => e -> SomeException
orchidExceptionToException = toException . SomeOrchidException

orchidExceptionFromException :: Exception e => SomeException -> Maybe e
orchidExceptionFromException x = do
    SomeOrchidException a <- fromException x
    cast a

data ParserException =
    ParserException ParseError
    deriving (Show, Typeable)

instance Exception ParserException where
    toException = orchidExceptionToException
    fromException = orchidExceptionFromException
    displayException (ParserException e) = show e

data CodegenException =
    CodegenException Text
    deriving (Show, Typeable)

instance Exception CodegenException where
    toException = orchidExceptionToException
    fromException = orchidExceptionFromException
    displayException (CodegenException e) = unpack e

data LowLevelException =
    LowLevelException Text
    deriving (Show,Typeable)

instance Exception LowLevelException where
    toException = orchidExceptionToException
    fromException = orchidExceptionFromException
    displayException (LowLevelException e) = unpack e

data FatalError =
    FatalError Text
    deriving (Show,Typeable)

instance Exception FatalError where
    toException = orchidExceptionToException
    fromException = orchidExceptionFromException
