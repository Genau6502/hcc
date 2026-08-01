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
        ]
    )