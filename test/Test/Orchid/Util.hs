-- | Utilities for tests.

module Test.Orchid.Util
       ( testPath
       ) where

testPath :: FilePath -> FilePath
testPath = ("test/data/" ++)
