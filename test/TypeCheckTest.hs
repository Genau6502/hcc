module TypeCheckTest(typeCheckTests) where

import TestTypes
import Types
import TypeCheck
import ParseContext

typeCheckTests :: TestGroup
typeCheckTests = TestGroup (
    "Type check tests", [
        "1+1 (ints)" -: parseTypeOfExpr emptyContext (AddExpr (IntAtom 1) (IntAtom 2)) --> Right IntType,
        "1+('a') (invalid - TODO)" -: parseTypeOfExpr emptyContext (AddExpr (IntAtom 1) (ParenAtom (AtomExpr (CharAtom 'a')))) --> Left (MismatchType IntType CharType)
        , "while(x) { return 0; } (invalid condition type)" -: typeCheckStmt (varContext [Var (PointerType IntType) "x"]) (WhileStmt (AtomExpr (VarAtom (Var (PointerType IntType) "x"))) [ReturnStmt (AtomExpr (IntAtom 0))]) --> Left (MismatchType IntType (PointerType IntType))
        , "while(x) { return 0; } (valid)" -: typeCheckStmt (varContext [Var IntType "x"]) (WhileStmt (AtomExpr (VarAtom (Var IntType "x"))) [ReturnStmt (AtomExpr (IntAtom 0))]) --> Right ()
        , "TypeCheck ExprStmt Valid" -: typeCheckStmt (varContext [Var IntType "x"]) (ExprStmt (AssignExpr (Var IntType "x") (AtomExpr (IntAtom 5)))) --> Right ()
        , "TypeCheck ExprStmt Mismatch" -: typeCheckStmt (varContext [Var IntType "x"]) (ExprStmt (AssignExpr (Var IntType "x") (AtomExpr (CharAtom 'c')))) --> Left (MismatchType IntType CharType)
        , "TypeCheck return statement valid" -: typeCheckStmt (funcContext IntType []) (ReturnStmt (AtomExpr (IntAtom 1))) --> Right ()
        , "TypeCheck return statement invalid" -: typeCheckStmt (funcContext CharType []) (ReturnStmt (AtomExpr (IntAtom 1))) --> Left (ReturnTypeMismatch CharType IntType)
        ]
    )