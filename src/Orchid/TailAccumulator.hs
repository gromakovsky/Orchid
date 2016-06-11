{-# LANGUAGE TupleSections #-}

-- | Convert recursive call into tail recursive one using accumulator
-- argument in some cases.

module Orchid.TailAccumulator
       ( toTailRecursive
       ) where

import           Data.Maybe   (catMaybes, fromJust, fromMaybe, isJust,
                               isNothing)

import           Orchid.Types (Atom (..), AtomExpr (..), BinaryOp (..),
                               CompoundStmt (..), DeclStmt (..), Expr (..),
                               ExprStmt (..), FlowStmt (..), FlowStmt (..),
                               FuncDef (..), Identifier, IfStmt (..),
                               ReturnStmt (..), SimpleStmt (..), SmallStmt (..),
                               Stmt (..), Suite (..), TypedArgument (..),
                               WhileStmt (..))

findReturns :: Suite -> [ReturnStmt]
findReturns (Suite stmts) = concatMap findReturnsStmt stmts
  where
    findReturnsStmt (SSimple (SimpleStmt ss)) =
        concatMap findReturnsSmallStmt ss
    findReturnsStmt (SCompound cs) = findReturnsCompoundStmt cs
    findReturnsSmallStmt (SSFlow (FSReturn s)) = [s]
    findReturnsSmallStmt _ = []
    findReturnsCompoundStmt (CSIf (IfStmt _ s1 s2)) =
        findReturns s1 ++ findReturns (fromMaybe mempty s2)
    findReturnsCompoundStmt (CSWhile (WhileStmt _ s1)) = findReturns s1
    findReturnsCompoundStmt _ = []

isReturnConstant :: ReturnStmt -> Maybe Expr
isReturnConstant (ReturnStmt Nothing) = Nothing
isReturnConstant (ReturnStmt (Just e))
  | isConstant e = Just e
  | otherwise = Nothing
  where
    isConstant (EAtom (AEAtom a)) = isConstantAtom a
    isConstant _ = False
    isConstantAtom (ANumber _) = True
    isConstantAtom (ABool _) = True
    isConstantAtom _ = False

isReturnAlmostTail :: Identifier -> ReturnStmt -> Bool
isReturnAlmostTail _ (ReturnStmt Nothing) = False
isReturnAlmostTail _ (ReturnStmt (Just (EUnary _ _))) = False
isReturnAlmostTail n (ReturnStmt (Just (EBinary op e1 e2))) =
    isReturnAlmostTailDo n op e1 e2
isReturnAlmostTail _ (ReturnStmt (Just (EAtom _))) = False

isReturnAlmostTailDo :: Identifier -> BinaryOp -> Expr -> Expr -> Bool
isReturnAlmostTailDo n op e1 e2
  | not (isGoodOp op) = False
  | isJust (isTailCall n e1) = True
  | isJust (isTailCall n e2) = True
  | otherwise = False
  where
    isGoodOp BinOr = True
    isGoodOp BinAnd = True
    isGoodOp BinPlus = True
    isGoodOp BinMult = True
    isGoodOp _ = False

isTailCall :: Identifier -> Expr -> Maybe [Expr]
isTailCall n (EAtom (AECall (AEAtom (AIdentifier n')) args))
  | n == n' = Just args
  | otherwise = Nothing
isTailCall _ _ = Nothing

modifySuite :: (Stmt -> Stmt) -> Suite -> Suite
modifySuite f = Suite . map f . getSuite

modifyStmt :: (SimpleStmt -> SimpleStmt) -> (CompoundStmt -> CompoundStmt) -> Stmt -> Stmt
modifyStmt f _ (SSimple s) = SSimple (f s)
modifyStmt _ g (SCompound s) = SCompound (g s)

modifySimpleStmt :: (SmallStmt -> SmallStmt) -> SimpleStmt -> SimpleStmt
modifySimpleStmt f (SimpleStmt ss) = SimpleStmt . map f $ ss

modifyCompoundStmt :: (SmallStmt -> SmallStmt) -> (Expr -> Expr) -> CompoundStmt -> CompoundStmt
modifyCompoundStmt f g (CSIf (IfStmt e s ms)) =
    CSIf $
    IfStmt (g e) (modifySuite stmtModifier s) (modifySuite stmtModifier <$> ms)
  where
    stmtModifier = modifyStmt simpleStmtModifier compoundStmtModifier
    simpleStmtModifier = modifySimpleStmt f
    compoundStmtModifier = modifyCompoundStmt f g
modifyCompoundStmt f g (CSWhile (WhileStmt e s)) =
    CSWhile $ WhileStmt (g e) (modifySuite stmtModifier s)
  where
    stmtModifier = modifyStmt simpleStmtModifier compoundStmtModifier
    simpleStmtModifier = modifySimpleStmt f
    compoundStmtModifier = modifyCompoundStmt f g
modifyCompoundStmt _ _ s = s

modifySmallStmt :: (Expr -> Expr) -> SmallStmt -> SmallStmt
modifySmallStmt f (SSDecl (DeclStmt t v e))= SSDecl $ DeclStmt t v (f e)
modifySmallStmt f (SSExpr (ExprStmt v e))= SSExpr $ ExprStmt v (f e)
modifySmallStmt f (SSFlow (FSReturn (ReturnStmt me))) =
    SSFlow $ FSReturn $ ReturnStmt (f <$> me)
modifySmallStmt _ s = s

changeFuncNameAndArgs :: Identifier -> Identifier -> Suite -> Suite
changeFuncNameAndArgs oldName newName = modifySuite stmtModifier
  where
    stmtModifier = modifyStmt simpleStmtModifier compoundStmtModifier
    simpleStmtModifier = modifySimpleStmt smallStmtModifier
    compoundStmtModifier = modifyCompoundStmt smallStmtModifier exprModifier
    smallStmtModifier = modifySmallStmt exprModifier
    exprModifier (EUnary op e) = EUnary op (exprModifier e)
    exprModifier (EBinary op e1 e2) =
        EBinary op (exprModifier e1) (exprModifier e2)
    exprModifier (EAtom ae) = EAtom $ atomExprModifer ae
    atomExprModifer (AEAtom a) = AEAtom $ atomModifier a
    atomExprModifer (AECall (AEAtom (AIdentifier n)) exprs)
      | n == oldName =
          AECall
              (AEAtom (AIdentifier newName))
              (map exprModifier exprs ++
               [EAtom $ AEAtom $ AIdentifier accumulatorName])
      | otherwise = AECall (AEAtom (AIdentifier n)) $ map exprModifier exprs
    atomExprModifer (AECall ae exprs) =
        AECall (atomExprModifer ae) $ map exprModifier exprs
    atomExprModifer (AEAccess ae i) = AEAccess (atomExprModifer ae) i
    atomModifier (AExpr e) = AExpr (exprModifier e)
    atomModifier a@(AIdentifier n)
      | n == oldName = AIdentifier newName
      | otherwise = a
    atomModifier c = c

changeReturnConstant :: Suite -> Suite
changeReturnConstant = modifySuite stmtModifier
  where
    stmtModifier = modifyStmt simpleStmtModifier compoundStmtModifier
    simpleStmtModifier = modifySimpleStmt smallStmtModifier
    compoundStmtModifier = modifyCompoundStmt smallStmtModifier id
    smallStmtModifier (SSFlow (FSReturn rs)) =
        SSFlow $ FSReturn $ returnStmtModifier rs
    smallStmtModifier s = s
    returnStmtModifier rs
      | isJust (isReturnConstant rs) =
          ReturnStmt $ Just $ EAtom $ AEAtom $ AIdentifier accumulatorName
      | otherwise = rs

changeReturnAlmostTail :: Identifier -> Suite -> Suite
changeReturnAlmostTail n = modifySuite stmtModifier
  where
    stmtModifier = modifyStmt simpleStmtModifier compoundStmtModifier
    simpleStmtModifier = modifySimpleStmt smallStmtModifier
    compoundStmtModifier = modifyCompoundStmt smallStmtModifier id
    smallStmtModifier (SSFlow (FSReturn rs)) =
        SSFlow $ FSReturn $ returnStmtModifier rs
    smallStmtModifier s = s
    returnStmtModifier rs
      | isReturnAlmostTail n rs = changeReturnAlmostTailDo n rs
      | otherwise = rs

changeReturnAlmostTailDo :: Identifier -> ReturnStmt -> ReturnStmt
changeReturnAlmostTailDo n (ReturnStmt (Just (EBinary op e1 e2))) =
    case isTailCall n e1 of
        Just args ->
            ReturnStmt $
            Just $
            EAtom $ AECall fAtomExpr $ args ++ [EBinary op accumulatorExpr e2]
        Nothing ->
            case isTailCall n e2 of
                Just args ->
                    ReturnStmt $
                    Just $
                    EAtom $
                    AECall fAtomExpr $ args ++ [EBinary op e1 accumulatorExpr]
                Nothing -> error "changeReturnAlmostTailDo"
  where
    fAtomExpr = AEAtom $ AIdentifier $ mangleFuncName n
changeReturnAlmostTailDo _ _ = error "changeReturnAlmostTailDo"

mangleFuncName :: Identifier -> Identifier
mangleFuncName n = mconcat [n, "$$acc"]

accumulatorName :: Identifier
accumulatorName = "acc$$"

accumulatorExpr :: Expr
accumulatorExpr = EAtom $ AEAtom $ AIdentifier accumulatorName

toTailRecursive :: FuncDef -> Maybe (FuncDef, FuncDef)
toTailRecursive FuncDef{..}
  | isNothing funcRet = Nothing
  | length constantReturns == 1 && length almostTailReturns == 1 =
      Just (extraFunc, wrapperFunc)
  | otherwise = Nothing
  where
    returns = findReturns funcBody
    constantReturns = catMaybes $ map isReturnConstant returns
    almostTailReturns = filter (isReturnAlmostTail funcName) returns
    fRet = fromJust funcRet
    wrapperFunc =
        FuncDef
        { funcName = funcName
        , funcArgs = funcArgs
        , funcRet = funcRet
        , funcBody = wrapperBody
        }
    extraName = mangleFuncName funcName
    wrapperBody =
        Suite
            [ SSimple $
              SimpleStmt [SSFlow $ FSReturn $ ReturnStmt $ Just wrapperSubExpr]]
    wrapperSubExpr =
        EAtom .
        AECall (AEAtom $ AIdentifier extraName) .
        (++ constantReturns) . map argToExpr $
        funcArgs
    argToExpr TypedArgument{..} = EAtom $ AEAtom $ AIdentifier taName
    extraFunc =
        FuncDef
        { funcName = extraName
        , funcArgs = funcArgs ++ [extraArg]
        , funcRet = funcRet
        , funcBody = extraBody
        }
    extraArg =
        TypedArgument
        { taName = accumulatorName
        , taType = fRet
        }
    extraBody =
        changeFuncNameAndArgs funcName extraName .
        changeReturnConstant . (changeReturnAlmostTail funcName) $
        funcBody
