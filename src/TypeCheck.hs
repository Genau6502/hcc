module TypeCheck(parseTypeOfExpr, typeCheckStmt) where

import Types
import ParseContext
import Control.Monad (unless)

(<=>) :: Either Error Type -> Either Error Type -> Either Error Type
Right x <=> Right y
    | x == y = Right x
    | otherwise = Left $ MismatchType x y
_ <=> _ = Left Unexpected

typeCheckStmt :: Context -> Stmt -> Either Error ()
typeCheckStmt ctx (DeclareAndAssignStmt (Var t _) e) = do
    exprT <- parseTypeOfExpr ctx e
    unless (exprT == t) $ Left (MismatchType t exprT)
typeCheckStmt ctx (WhileStmt e block) = do
    exprT <- parseTypeOfExpr ctx e
    unless (exprT == IntType) $ Left (MismatchType IntType exprT)
typeCheckStmt ctx (ExprStmt e) = (const ()) <$> parseTypeOfExpr ctx e
typeCheckStmt ctx (ReturnStmt e) = do
    exprT <- parseTypeOfExpr ctx e
    (const ()) <$> checkFunctionReturnType ctx exprT

parseTypeOfExpr :: Context -> Expr -> Either Error Type
parseTypeOfExpr ctx (AddExpr x y) = atomSameType ctx x y
parseTypeOfExpr ctx (SubtractExpr x y) = atomSameType ctx x y
parseTypeOfExpr ctx (MultiplyExpr x y) = atomSameType ctx x y
parseTypeOfExpr ctx (AtomExpr a) = parseTypeOfAtom ctx a
parseTypeOfExpr ctx (AssignExpr (Var t _) e) = do
    exprT <- parseTypeOfExpr ctx e
    unless (exprT == t) $ Left (MismatchType t exprT)
    return t

atomSameType :: Context -> Atom -> Atom -> Either Error Type
atomSameType ctx x y = parseTypeOfAtom ctx x <=> parseTypeOfAtom ctx y

parseTypeOfAtom :: Context -> Atom -> Either Error Type
parseTypeOfAtom _ (IntAtom _) = pure IntType
parseTypeOfAtom _ (VarAtom (Var t _)) = pure t
parseTypeOfAtom _ (CharAtom _) = pure CharType
parseTypeOfAtom ctx (ParenAtom e) = parseTypeOfExpr ctx e
parseTypeOfAtom _ (CastAtom t e) = pure t
parseTypeOfAtom _ _ = Left $ Unexpected
