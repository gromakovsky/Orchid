-- | Utilities for tests.

module Orchid.Util
       ( testPath
       ) where

testPath :: FilePath -> FilePath
testPath = ("test/data/" ++)
