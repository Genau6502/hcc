module RegisterTest where

import TestTypes
import Types
import Registers

allocateRegistersTests :: TestGroup
allocateRegistersTests = TestGroup 
    (
        "Register allocation tests"
        , [   "(1)" -: allocateRegistersForBlock [] [] [DeclareAndAssignStmt (Var IntType "x") (AtomExpr (IntAtom 2)), DeclareAndAssignStmt (Var IntType "y") (AtomExpr (IntAtom 1))] --> [(Var IntType "y",R12),(Var IntType "x",R12)]
        ]
    )