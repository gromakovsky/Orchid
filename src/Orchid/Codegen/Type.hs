-- | Type type.

module Orchid.Codegen.Type
       ( Type (..)
       ) where

import           Data.Text           (Text)
import           Data.Text.Buildable (Buildable (build))

-- | Types supported by Codegen.
data Type
    = TInt64
    | TBool
    | TVoid
    | TPointer Type
    | TFunction Type [Type]
    | TClass Text
             [Type]
    deriving (Show, Eq)

instance Buildable Type where
    build = build . show
