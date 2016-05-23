{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Constants creation.

module Orchid.Codegen.Constant
       ( constInt64
       , constInt32
       , constBool
       , ToConstant (toConstant)
       , complexConstant
       ) where

import           Control.Lens              (at, use, view)
import           Control.Monad.Except      (MonadError)
import           Control.Monad.State       (MonadState)
import           Data.Int                  (Int32, Int64)
import qualified Data.Map                  as M
import           Data.Text                 (Text)
import qualified LLVM.General.AST.Constant as C

import           Serokell.Util             (formatSingle')

import           Orchid.Codegen.Common     (HasCodegen (classesLens),
                                            cdVariables, cvInitializer,
                                            throwCodegenError)
import           Orchid.Codegen.Type       (Type (..))
import           Orchid.Error              (CodegenException)

constInt64 :: Int64 -> C.Constant
constInt64 = C.Int 64 . toInteger

constInt32 :: Int32 -> C.Constant
constInt32 = C.Int 32 . toInteger

constBool :: Bool -> C.Constant
constBool = C.Int 1 . boolToInteger
  where
    boolToInteger False = 0
    boolToInteger True = 1

class ToConstant a  where
    toConstant
        :: (MonadError CodegenException m, MonadState s m, HasCodegen s)
        => a -> m C.Constant

instance ToConstant Int64 where
    toConstant = pure . constInt64

instance ToConstant Bool where
    toConstant = pure . constBool

-- | Complex constant is produced by constructor.
complexConstant
    :: (MonadError CodegenException m, MonadState s m, HasCodegen s)
    => Text -> [(Type, C.Constant)] -> m C.Constant
complexConstant constructorName [] = do
    cls <- use $ classesLens . at constructorName
    maybe onFailure onSuccess cls
  where
    onFailure =
        throwCodegenError $
        formatSingle' "constructor not found: {}" constructorName
    onSuccess =
        return .
        C.Struct Nothing False .
        map (view cvInitializer) . M.elems . view cdVariables
complexConstant _ _ = throwCodegenError "constructors with arguments are not supported"
