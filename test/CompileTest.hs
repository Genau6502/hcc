module CompileTest where

import TestTypes
import Types
import Registers
import Compile 
import Instructions 

compileBlockTests :: TestGroup
compileBlockTests = TestGroup 
    (
        "Compile tests"
        , [   
              "(1) Empty block compilation" -: 
                compileBlock [] [] 0 [] 
                --> ([], 0)

            , "(2) Single declare and assign statement" -: 
                let 
                    vX = Var IntType "x"
                    stmts = [DeclareAndAssignStmt vX (AtomExpr (IntAtom 0))]
                    lvs = []
                in
                    compileBlock lvs (allocateRegistersForBlock lvs [] stmts) 0 stmts 
                    --> ([MOV L (Immediate 0) R12,MOV L R12 R12], 0)

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
                    compileBlock lvs ra 0 stmts 
                    --> ([MOV L (Immediate 2) R12,MOV L R12 R12,MOV L R12 R13], 0)

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
                    compileBlock lvs ra 0 stmts 
                    --> ([MOV L (Immediate 2) R13, MOV L R13 R14,MOV L (Immediate 3) R13,MOV L R13 R15,MOV L R15 R12], 0)

            , "(5) Statement requiring two dummy variables (binary expression)" -: compileBlock [] (allocateRegistersForBlock [] [] [DeclareAndAssignStmt (Var IntType "z") (AddExpr (IntAtom 2) (IntAtom 3))]) 0 [DeclareAndAssignStmt (Var IntType "z") (AddExpr (IntAtom 2) (IntAtom 3))] --> ([MOV L (Immediate 2) R12, MOV L (Immediate 3) R13, ADD L R12 R13 R14, MOV L R14 R12], 0)
            , "(6) Subtraction with two literals" -: compileBlock [] [(Var IntType "x", R12)] 0 [DeclareAndAssignStmt (Var IntType "x") (SubtractExpr (IntAtom 5) (IntAtom 3))] --> ([MOV L (Immediate 5) R12, MOV L (Immediate 3) R13, SUB L R12 R13 R14, MOV L R14 R12], 0)
            , "(7) Multiplication of literal and live variable" -: compileBlock [Var IntType "x"] [(Var IntType "x", R12), (Var IntType "y", R13)] 0 [DeclareAndAssignStmt (Var IntType "y") (MultiplyExpr (IntAtom 4) (VarAtom (Var IntType "x")))] --> ([MOV L (Immediate 4) R13, IMUL L R13 R12 R14, MOV L R14 R13], 0)
            , "(8) While loop with literal condition and empty body" -: 
                let 
                    -- while (1) {}
                    expr = AtomExpr (IntAtom 1)
                    block = []
                    lvs = []
                    ra = []
                    startLabelId = 0
                in
                    compileStmt lvs ra startLabelId (WhileStmt expr block) 
                    --> ( [ Label 0
                          , MOV L (Immediate 1) R12 -- Assuming IntAtom 1 allocates dummy R12
                          , TEST L R12 R12
                          , JE (Label 1)
                          -- (Body is empty, so no instructions here)
                          , JMP (Label 0)
                          , Label 1
                          ]
                        , 2 ) -- Block consumed 0 labels, so it returns i+2

            , "(9) While loop with live variable condition and assignment body" -: 
                let 
                    -- while (x) { int y = 2; }
                    vX = Var IntType "x"
                    vY = Var IntType "y"
                    expr = AtomExpr (VarAtom vX)
                    block = [DeclareAndAssignStmt vY (AtomExpr (IntAtom 2))]
                    
                    lvs = [vX]
                    ra = [(vX, R12)]
                    startLabelId = 4
                in
                    compileStmt lvs ra startLabelId (WhileStmt expr block) 
                    --> ( [ Label 4
                          , TEST L R12 R12
                          , JE (Label 5)
                          , MOV L (Immediate 2) R13
                          , MOV L R13 R13
                          , JMP (Label 4)
                          , Label 5
                          ]
                        , 6 )
            , "(10) Single ExprStmt with AssignExpr (x = 5;)" -: compileBlock [Var IntType "x"] [(Var IntType "x", R12)] 0 [ExprStmt (AssignExpr (Var IntType "x") (AtomExpr (IntAtom 5)))] --> ([MOV L (Immediate 5) R13, MOV L R13 R12], 0)
            , "(11) Assigning one variable to another (x = y;)" -: compileBlock [Var IntType "x", Var IntType "y"] [(Var IntType "x", R12), (Var IntType "y", R13)] 0 [ExprStmt (AssignExpr (Var IntType "x") (AtomExpr (VarAtom (Var IntType "y"))))] --> ([MOV L R13 R12], 0)
            , "(12) Chained assignments (x = y = 5;)" -: compileBlock [Var IntType "x", Var IntType "y"] [(Var IntType "x", R12), (Var IntType "y", R13)] 0 [ExprStmt (AssignExpr (Var IntType "x") (AssignExpr (Var IntType "y") (AtomExpr (IntAtom 5))))] --> ([MOV L (Immediate 5) R14, MOV L R14 R13, MOV L R13 R12], 0)
            , "(13) Assignment with arithmetic expression (x = y + 3;)" -: compileBlock [Var IntType "x", Var IntType "y"] [(Var IntType "x", R12), (Var IntType "y", R13)] 0 [ExprStmt (AssignExpr (Var IntType "x") (AddExpr (VarAtom (Var IntType "y")) (IntAtom 3)))] --> ([MOV L (Immediate 3) R14, ADD L R13 R14 R15, MOV L R15 R12], 0)        ]
    ) 
