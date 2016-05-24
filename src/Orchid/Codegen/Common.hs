{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | Common definitions for Codegen modules.

module Orchid.Codegen.Common
       ( throwCodegenError
       , Names
       , uniqueName
       , thisPtrName
       , orchidTypeToLLVM
       , mangleClassMethodName
       , TypedOperand
       , VariableData (..)
       , variableDataToTypedOperand
       , VariablesMap
       , FunctionData (..)
       , functionDataToType
       , typeToFunctionData
       , FunctionsMap
       , ClassVariable
       , mkClassVariable
       , cvName
       , cvType
       , cvInitializer
       , cvPrivate
       , ClassData
       , mkClassData
       , cdVariables
       , cdParent
       , ClassesMap
       , HasClasses (..)
       , lookupType
       , lookupClassType
       , classAndParents
       , parentClass
       , isSubClass
       ) where

import           Control.Lens               (Lens', at, makeLenses, use, view)
import           Control.Monad.Except       (MonadError (throwError))
import           Control.Monad.State        (MonadState)
import qualified Data.Map                   as M
import           Data.String                (IsString (fromString))
import           Data.String.Conversions    (ConvertibleStrings (convertString))
import           Data.Text                  (Text)
import           Data.Text.Buildable        (Buildable (build))
import qualified LLVM.General.AST           as AST
import           LLVM.General.AST.AddrSpace (AddrSpace (AddrSpace))
import qualified LLVM.General.AST.Constant  as C
import qualified LLVM.General.AST.Type      as T (Type (..), i1, i64, void)

import           Serokell.Util              (formatSingle', show')

import           Orchid.Codegen.Type        (Type (..))
import           Orchid.Error               (CodegenException (CodegenException))

-------------------------------------------------------------------------------
-- Useful instances
-------------------------------------------------------------------------------

instance IsString AST.Name where
    fromString = AST.Name . fromString

instance Buildable AST.Name where
    build (AST.Name a) = build a
    build (AST.UnName a) = build a

instance ConvertibleStrings String AST.Name where
    convertString = AST.Name

instance ConvertibleStrings Text AST.Name where
    convertString = AST.Name . convertString

-------------------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------------------

throwCodegenError
    :: MonadError CodegenException m
    => Text -> m a
throwCodegenError = throwError . CodegenException

type Names = M.Map Text Int

uniqueName :: Text -> Names -> (Text, Names)
uniqueName nm ns =
    case M.lookup nm ns of
        Nothing -> (nm, M.insert nm 1 ns)
        Just ix -> (mconcat [nm, show' ix], M.insert nm (ix + 1) ns)

-------------------------------------------------------------------------------
-- Common functions
-------------------------------------------------------------------------------

thisPtrName :: IsString s => s
thisPtrName = "this"

orchidTypeToLLVM :: Type -> T.Type
orchidTypeToLLVM TInt64 = T.i64
orchidTypeToLLVM TBool = T.i1
orchidTypeToLLVM TVoid = T.void
orchidTypeToLLVM (TPointer t) =
    T.PointerType (orchidTypeToLLVM t) (AddrSpace 0)
orchidTypeToLLVM (TFunction retType argTypes) =
    T.FunctionType
        (orchidTypeToLLVM retType)
        (map orchidTypeToLLVM argTypes)
        False
orchidTypeToLLVM (TClass _ types) =
    T.StructureType False $ map orchidTypeToLLVM types

mangleClassMethodName :: (IsString s, Monoid s) => s -> s -> s
mangleClassMethodName className funcName = mconcat [className, "$$", funcName]

-------------------------------------------------------------------------------
-- Common types
-------------------------------------------------------------------------------

type TypedOperand = (Type, AST.Operand)

-- | VariableData type represents information about variable stored somewhere.
data VariableData = VariableData
    { vdType       :: Type         -- ^ Type of variable
    , vdPtrOperand :: AST.Operand  -- ^ Pointer operand (it's type is
                                   -- a pointer to vdType)
    } deriving (Show)

variableDataToTypedOperand :: VariableData -> TypedOperand
variableDataToTypedOperand VariableData {..} = (TPointer vdType, vdPtrOperand)

type VariablesMap = M.Map Text VariableData

-- | FunctionData represents information about function.
-- It doesn't store Operand because it's easily constructible from
-- function name.
data FunctionData = FunctionData
    { fdRetType  :: Type
    , fdArgTypes :: [Type]
    } deriving (Show)

functionDataToType :: FunctionData -> Type
functionDataToType FunctionData{..} = TFunction fdRetType fdArgTypes

typeToFunctionData :: Type -> Maybe FunctionData
typeToFunctionData (TFunction retType argTypes) =
    Just $ FunctionData retType argTypes
typeToFunctionData _ = Nothing

type FunctionsMap = M.Map Text FunctionData

-- | ClassVariable type represents variable defined inside class
-- body. It has a names, type, initial value (used for construction)
-- and access modifier.
data ClassVariable = ClassVariable
    { _cvName        :: Text
    , _cvType        :: Type
    , _cvInitializer :: C.Constant
    , _cvPrivate     :: Bool
    } deriving (Show)

$(makeLenses ''ClassVariable)

mkClassVariable :: Text -> Type -> C.Constant -> Bool -> ClassVariable
mkClassVariable = ClassVariable

-- | ClassData stores all data associated with class.
data ClassData = ClassData
    { _cdVariables :: [ClassVariable]
    , _cdParent    :: Maybe Text
    } deriving (Show)

$(makeLenses ''ClassData)

mkClassData :: [ClassVariable] -> Maybe Text -> ClassData
mkClassData = ClassData

type ClassesMap = M.Map Text ClassData

-- | HasClasses type class abstracts over types which provide access
-- to ClassesMap.
class HasClasses s  where
    classesLens :: Lens' s ClassesMap

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

predefinedTypes :: M.Map Text Type
predefinedTypes = M.fromList [("int64", TInt64), ("bool", TBool)]

classType :: Text -> ClassData -> Type
classType className = TClass className . map (view cvType) . view cdVariables

throwUnknownType :: (MonadError CodegenException m) => Text -> m a
throwUnknownType = throwCodegenError . formatSingle' "unknown type: {}"

lookupType
    :: (MonadError CodegenException m, MonadState s m, HasClasses s)
    => Text -> m Type
lookupType t = maybe (lookupClassType t) return $ M.lookup t predefinedTypes

lookupClassType
    :: (MonadError CodegenException m, MonadState s m, HasClasses s)
    => Text -> m Type
lookupClassType t =
    maybe (throwUnknownType t) (return . classType t) =<< use (classesLens . at t)

classAndParents
    :: (MonadState s m, HasClasses s)
    => Maybe Text -> m [Text]
classAndParents Nothing = pure []
classAndParents (Just className) = do
    p <- parentClass className
    (className :) <$> classAndParents p

-- | This function returns name of parent class of the given class (if
-- any).
parentClass
    :: (MonadState s m, HasClasses s)
    => Text -> m (Maybe Text)
parentClass className =
    (>>= view cdParent) <$> use (classesLens . at className)

-- | This function tests whether class identified by the first
-- argument is a subclass of the class identified by the second
-- argument.
isSubClass
    :: (MonadState s m, HasClasses s)
    => Text -> Text -> m Bool
isSubClass subC superC = elem superC <$> classAndParents (Just subC)
