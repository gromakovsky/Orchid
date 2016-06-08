{-# LANGUAGE TemplateHaskell #-}

-- | Test data.

module Test.Orchid.Data
       ( factorial_ioSource
       , classSource
       , errorSource
       , global_varSource
       , type_errorSource
       , rectangleSource
       , private_var_insideSource
       , private_var_outsideSource
       , class_method_insideSource
       , private_method_insideSource
       , private_method_outsideSource
       , inheritanceSource
       , pointerSource
       , shapeSource
       , virtualSource
       ) where

import           Data.FileEmbed   (embedStringFile)
import           Data.String      (IsString)

import           Test.Orchid.Util (testPath)

factorial_ioSource
    :: IsString s
    => s
factorial_ioSource = $(embedStringFile $ testPath "factorial_io.orc")

classSource
    :: IsString s
    => s
classSource = $(embedStringFile $ testPath "class.orc")

errorSource
    :: IsString s
    => s
errorSource = $(embedStringFile $ testPath "error.orc")

global_varSource
    :: IsString s
    => s
global_varSource = $(embedStringFile $ testPath "global_var.orc")

type_errorSource
    :: IsString s
    => s
type_errorSource = $(embedStringFile $ testPath "type_error.orc")

rectangleSource
    :: IsString s
    => s
rectangleSource = $(embedStringFile $ testPath "rectangle.orc")

private_var_insideSource
    :: IsString s
    => s
private_var_insideSource = $(embedStringFile $ testPath "private_var_inside.orc")

private_var_outsideSource
    :: IsString s
    => s
private_var_outsideSource = $(embedStringFile $ testPath "private_var_outside.orc")

class_method_insideSource
    :: IsString s
    => s
class_method_insideSource = $(embedStringFile $ testPath "class_method_inside.orc")

private_method_insideSource
    :: IsString s
    => s
private_method_insideSource = $(embedStringFile $ testPath "private_method_inside.orc")

private_method_outsideSource
    :: IsString s
    => s
private_method_outsideSource = $(embedStringFile $ testPath "private_method_outside.orc")

inheritanceSource
    :: IsString s
    => s
inheritanceSource = $(embedStringFile $ testPath "inheritance.orc")

pointerSource
    :: IsString s
    => s
pointerSource = $(embedStringFile $ testPath "pointer.orc")

shapeSource
    :: IsString s
    => s
shapeSource = $(embedStringFile $ testPath "shape.orc")

virtualSource
    :: IsString s
    => s
virtualSource = $(embedStringFile $ testPath "virtual.orc")
