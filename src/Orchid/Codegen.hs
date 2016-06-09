{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE TemplateHaskell        #-}

-- | LLVM code generation.

module Orchid.Codegen
       (
         module Exports
       , AST.Name (..)
       ) where

import           Orchid.Codegen.Body     as Exports
import           Orchid.Codegen.Common   as Exports (FunctionData (..),
                                                     HasClasses, TypedOperand,
                                                     lookupClassType,
                                                     lookupType,
                                                     mkClassVariable,
                                                     throwCodegenError)
import           Orchid.Codegen.Constant as Exports hiding (constInt32)
import           Orchid.Codegen.Module   as Exports
import           Orchid.Codegen.Type     as Exports

import qualified LLVM.General.AST        as AST
