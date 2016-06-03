{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Constants creation.

module Orchid.Codegen.Constant
       ( constInt64
       , constInt32
       , constBool
       , ToConstant (toConstant, toConstantOperand)
       , complexConstant
       ) where

import           Control.Lens              (at, use, view)
import           Control.Monad.Except      (MonadError)
import           Control.Monad.State       (MonadState)
import           Data.Int                  (Int32, Int64)
import           Data.String.Conversions   (convertString)
import           Data.Text                 (Text)
import qualified LLVM.General.AST          as AST
import qualified LLVM.General.AST.Constant as C

import           Serokell.Util             (formatSingle')

import           Orchid.Codegen.Common     (HasClasses (classesLens),
                                            cdVariables, cvInitializer,
                                            throwCodegenError, vTableName,
                                            vTableTypeName)
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
        :: (MonadError CodegenException m, MonadState s m, HasClasses s)
        => a -> m C.Constant
    toConstantOperand
        :: (MonadError CodegenException m, MonadState s m, HasClasses s)
        => a -> m AST.Operand
    toConstantOperand = fmap AST.ConstantOperand . toConstant

instance ToConstant Int64 where
    toConstant = pure . constInt64

instance ToConstant Bool where
    toConstant = pure . constBool

-- | Complex constant is produced by constructor.
complexConstant
    :: (MonadError CodegenException m, MonadState s m, HasClasses s)
    => Text -> [(Type, C.Constant)] -> m C.Constant
complexConstant className [] = do
    cls <- use $ classesLens . at className
    maybe onFailure onSuccess cls
  where
    vTableType' =
        AST.NamedTypeReference $ convertString $ vTableTypeName className
    vTableConstant =
        C.GlobalReference vTableType' (convertString $ vTableName className)
    onFailure =
        throwCodegenError $ formatSingle' "constructor not found: {}" className
    onSuccess =
        return .
        C.Struct (Just . convertString $ className) False .
        (vTableConstant :) . map (view cvInitializer) . view cdVariables
complexConstant _ _ = throwCodegenError "constructors with arguments are not supported"
