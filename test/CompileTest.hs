module CompileTest where

import TestTypes
import Types
import Registers
import Compile 
import Instructions 

compileBlockTests :: TestGroup
compileBlockTests = TestGroup 
    (
        "compileBlock tests (DeclareAndAssignStmt only)"
        , [   
              "(1) Empty block compilation" -: 
                compileBlock [] [] [] 
                --> []

            , "(2) Single declare and assign statement" -: 
                let 
                    vX = Var IntType "x"
                    stmts = [DeclareAndAssignStmt vX (AtomExpr (IntAtom 0))]
                in
                    compileBlock [vX] (allocateRegisters stmts) stmts 
                    --> [MOV_I L 0 R13,MOV L R13 R12]

            , "(3) Sequential statements with live variable carried over" -: 
                let 
                    vX = Var IntType "x"
                    vY = Var IntType "y"
                    -- x is used in the assignment of y, so x remains live during the first step
                    stmts = [ DeclareAndAssignStmt vX (AtomExpr (IntAtom 2))
                            , DeclareAndAssignStmt vY (AtomExpr (VarAtom vX))
                            ]
                    ra = allocateRegisters stmts
                    lvs = [vX]
                in
                    compileBlock lvs ra stmts 
                    --> [MOV_I L 2 R13,MOV L R13 R12,MOV L R12 R12]

            , "(4) Sequential statements where a variable dies immediately" -: 
                let 
                    vX = Var IntType "x"
                    vY = Var IntType "y"
                    vZ = Var IntType "z"
                    -- vX is declared but never used in subsequent statements. 
                    -- clearDeadVars should strip it before compiling the second statement,
                    -- theoretically freeing up its register if the allocator is called.
                    stmts = [ DeclareAndAssignStmt vX (AtomExpr (IntAtom 2))
                            , DeclareAndAssignStmt vY (AtomExpr (IntAtom 3))
                            , DeclareAndAssignStmt vZ (AtomExpr (VarAtom vY))
                            ]
                    ra = [(vX, R12), (vY, R13), (vZ, R14)]
                    lvs = [vX, vY]
                in
                    compileBlock lvs ra stmts 
                    --> [MOV_I L 2 R14,MOV L R14 R12,MOV_I L 3 R13,MOV L R13 R13,MOV L R13 R14]
        ]
    )