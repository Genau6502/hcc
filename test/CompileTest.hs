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
                    lvs = []
                in
                    compileBlock lvs (allocateRegistersForBlock lvs [] stmts) stmts 
                    --> [MOV_I L 0 R12,MOV L R12 R12]

            , "(3) Sequential statements with live variable carried over" -: 
                let 
                    vX = Var IntType "x"
                    vY = Var IntType "y"
                    lvs = []
                    -- x is used in the assignment of y, so x remains live during the first step
                    stmts = [ DeclareAndAssignStmt vX (AtomExpr (IntAtom 2))
                            , DeclareAndAssignStmt vY (AtomExpr (VarAtom vX))
                            ]
                    ra = allocateRegistersForBlock lvs [] stmts
                in
                    compileBlock lvs ra stmts 
                    --> [MOV_I L 2 R12,MOV L R12 R12,MOV L R12 R12]

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
                    ra = allocateRegistersForBlock [] [] stmts
                    lvs = [vX, vY, vZ]
                in
                    compileBlock lvs ra stmts 
                    --> [MOV_I L 2 R13, MOV L R13 R14,MOV_I L 3 R12,MOV L R12 R13,MOV L R13 R12]
            , "(5) Statement requiring two dummy variables (binary expression)" -: 
                let 
                    vZ = Var IntType "z"
                    stmts = [DeclareAndAssignStmt vZ (AddExpr (IntAtom 2) (IntAtom 3))]
                    
                    -- Let's say z gets mapped to R12.
                    lvs = [] 
                    ra = allocateRegistersForBlock lvs [] stmts
                in
                    compileBlock lvs ra stmts 
                    --> [MOV_I L 2 R12,MOV_I L 3 R13,ADD L R12 R13 R12,MOV L R12 R12]
            , "(6) Subtraction with two literals" -: 
                let 
                    vX = Var IntType "x"
                    stmts = [DeclareAndAssignStmt vX (SubtractExpr (IntAtom 5) (IntAtom 3))]
                    
                    -- Assuming x gets allocated to R12
                    ra = [(vX, R12)]
                    lvs = [] 
                in
                    compileBlock lvs ra stmts 
                    --> [ MOV_I L 5 R12
                        , MOV_I L 3 R13
                        , SUB L R12 R13 R12
                        , MOV L R12 R12 
                        ]

            , "(7) Multiplication of literal and live variable" -: 
                let 
                    vX = Var IntType "x"
                    vY = Var IntType "y"
]                    stmts = [DeclareAndAssignStmt vY (MultiplyExpr (IntAtom 4) (VarAtom vX))]
                    
                    -- x is live in R12, y is allocated to R13
                    ra = [(vX, R12), (vY, R13)]
                    lvs = [vX] 
                in
                    compileBlock lvs ra stmts 
                    --> [ MOV_I L 4 R13
                        , IMUL L R13 R12 R13
                        , MOV L R13 R13
                        ]
        ]
    )