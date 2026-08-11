module TypeCheck(parseTypeOfExpr, typeCheckStmt, typeOfAtom, typeOfExpr) where

import Types
import ParseContext

(<=>) :: Either Error Type -> Either Error Type -> Either Error Type
Right x <=> Right y
    | x == y = Right x
    | otherwise = Left $ MismatchType x y
_ <=> _ = Left Unexpected

typeCheckStmt :: Context -> Stmt -> Either Error ()
typeCheckStmt ctx (DeclareAndAssignStmt (Var t _) e) = ((==t) <$> parseTypeOfExpr ctx e) >>= (const $ pure ())
typeCheckStmt ctx (AssignStmt (Var t _) e) = ((==t) <$> parseTypeOfExpr ctx e) >>= (const $ pure ())

parseTypeOfExpr :: Context -> Expr -> Either Error Type
parseTypeOfExpr ctx (AddExpr x y) = atomSameType ctx x y
parseTypeOfExpr ctx (SubtractExpr x y) = atomSameType ctx x y
parseTypeOfExpr ctx (MultiplyExpr x y) = atomSameType ctx x y
parseTypeOfExpr ctx (AtomExpr x) = parseTypeOfAtom ctx x

atomSameType :: Context -> Atom -> Atom -> Either Error Type
atomSameType ctx x y = parseTypeOfAtom ctx x <=> parseTypeOfAtom ctx y

parseTypeOfAtom :: Context -> Atom -> Either Error Type
parseTypeOfAtom _ (IntAtom _) = pure IntType
parseTypeOfAtom _ (VarAtom (Var t _)) = pure t
parseTypeOfAtom _ (CharAtom _) = pure CharType
parseTypeOfAtom ctx (ParenAtom e) = parseTypeOfExpr ctx e
parseTypeOfAtom _ (CastAtom t e) = pure t
parseTypeOfAtom _ _ = Left $ Unexpected

typeOfAtom :: Context -> Atom -> Type
typeOfAtom _ (IntAtom _) = IntType
typeOfAtom _ (VarAtom (Var t _)) = t
typeOfAtom _ (CharAtom _) = CharType
typeOfAtom ctx (ParenAtom e) = typeOfExpr ctx e
typeOfAtom _ (CastAtom t e) = t

typeOfExpr :: Context -> Expr -> Type
typeOfExpr ctx (AddExpr x y) = typeOfAtom ctx x
typeOfExpr ctx (SubtractExpr x y) = typeOfAtom ctx x
typeOfExpr ctx (MultiplyExpr x y) = typeOfAtom ctx x
typeOfExpr ctx (AtomExpr x) = typeOfAtom ctx x
