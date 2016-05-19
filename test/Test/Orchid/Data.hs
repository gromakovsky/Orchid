{-# LANGUAGE TemplateHaskell #-}

-- | Test data.

module Test.Orchid.Data
       ( factorial_ioInput
       , classInput
       , errorInput
       , type_errorInput
       , rectangleInput
       , private_var_insideInput
       , private_var_outsideInput
       , class_method_insideInput
       , private_method_insideInput
       , private_method_outsideInput
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

rectangleInput
    :: IsString s
    => s
rectangleInput = $(embedStringFile $ testPath "rectangle.orc")

private_var_insideInput
    :: IsString s
    => s
private_var_insideInput = $(embedStringFile $ testPath "private_var_inside.orc")

private_var_outsideInput
    :: IsString s
    => s
private_var_outsideInput = $(embedStringFile $ testPath "private_var_outside.orc")

class_method_insideInput
    :: IsString s
    => s
class_method_insideInput = $(embedStringFile $ testPath "class_method_inside.orc")

private_method_insideInput
    :: IsString s
    => s
private_method_insideInput = $(embedStringFile $ testPath "private_method_inside.orc")

private_method_outsideInput
    :: IsString s
    => s
private_method_outsideInput = $(embedStringFile $ testPath "private_method_outside.orc")
