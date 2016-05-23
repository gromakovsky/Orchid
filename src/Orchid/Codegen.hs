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
                                                     TypedOperand,
                                                     classPointerType,
                                                     lookupType,
                                                     mangleClassMethodName,
                                                     orchidTypeToLLVM,
                                                     thisPtrName,
                                                     throwCodegenError)
import           Orchid.Codegen.Constant as Exports
import           Orchid.Codegen.Module   as Exports
import           Orchid.Codegen.Type     as Exports

import qualified LLVM.General.AST        as AST
