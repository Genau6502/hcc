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
        , "while(x) { return 0; } (invalid condition type)" -: typeCheckStmt [Var (PointerType IntType) "x"] (WhileStmt (AtomExpr (VarAtom (Var (PointerType IntType) "x"))) [ReturnStmt (AtomExpr (IntAtom 0))]) --> Left (MismatchType IntType (PointerType IntType))
        , "while(x) { return 0; } (valid)" -: typeCheckStmt [Var IntType "x"] (WhileStmt (AtomExpr (VarAtom (Var IntType "x"))) [ReturnStmt (AtomExpr (IntAtom 0))]) --> Right ()
        ]
    )