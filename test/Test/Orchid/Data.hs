{-# LANGUAGE TemplateHaskell #-}

-- | Test data.

module Test.Orchid.Data
       ( factorial_ioInput
       , classInput
       , errorInput
       , type_errorInput
       ) where

import           Data.FileEmbed   (embedStringFile)
import           Data.String      (IsString)

import           Test.Orchid.Util (testPath)

factorial_ioInput
    :: IsString s
    => s
factorial_ioInput = $(embedStringFile $ testPath "factorial_io.orc")

classInput
    :: IsString s
    => s
classInput = $(embedStringFile $ testPath "class.orc")

errorInput
    :: IsString s
    => s
errorInput = $(embedStringFile $ testPath "error.orc")

type_errorInput
    :: IsString s
    => s
type_errorInput = $(embedStringFile $ testPath "type_error.orc")
