module CompileTest where

import TestTypes
import Types
import Registers
import Compile 
import Instructions 
import Data.List((\\))

-- Pre: all variables are not stack-allocated
makeRA :: [(Var, Location)] -> RegisterAllocation
makeRA allocs = emptyRA { allocations = allocs, freeregs = freeregs emptyRA \\ map snd allocs}

compileBlockTests :: TestGroup
compileBlockTests = TestGroup 
    (
        "Compile tests"
        , [   
              "(1) Empty block compilation" -: 
                compileBlock [] emptyRA 0 [] 
                --> ([], 0)

            , "(2) Single declare and assign statement" -: 
                let 
                    vX = Var IntType "x"
                    stmts = [DeclareAndAssignStmt vX (AtomExpr (IntAtom 0))]
                    lvs = []
                in
                    compileBlock lvs emptyRA 0 stmts
                    --> ([MOV L (Immediate 0) R13,MOV L R13 R12], 0)

            , "(3) Sequential statements with live variable carried over" -: 
                let 
                    vX = Var IntType "x"
                    vY = Var IntType "y"
                    lvs = []
                    -- x is used in the assignment of y, so x remains live during the first step
                    stmts = [ DeclareAndAssignStmt vX (AtomExpr (IntAtom 2))
                            , DeclareAndAssignStmt vY (AtomExpr (VarAtom vX))
                            ]
                in
                    compileBlock lvs emptyRA 0 stmts 
                    --> ([MOV L (Immediate 2) R13, MOV L R13 R12,MOV L R12 R13], 0)

            , "(4) Sequential statements where a variable dies immediately" -: compileBlock [Var IntType "x", Var IntType "y", Var IntType "z"] emptyRA 0 [DeclareAndAssignStmt (Var IntType "x") (AtomExpr (IntAtom 2)), DeclareAndAssignStmt (Var IntType "y") (AtomExpr (IntAtom 3)), DeclareAndAssignStmt (Var IntType "z") (AtomExpr (VarAtom (Var IntType "y")))] --> ([MOV L (Immediate 2) R13, MOV L R13 R12, MOV L (Immediate 3) R14, MOV L R14 R13, MOV L R13 R14], 0)
            , "(5) Statement requiring two dummy variables (binary expression)" -: compileBlock [] emptyRA 0 [DeclareAndAssignStmt (Var IntType "z") (AddExpr (IntAtom 2) (IntAtom 3))] --> ([MOV L (Immediate 2) R13, MOV L (Immediate 3) R14, ADD L R13 R14 R15, MOV L R15 R12], 0)
            , "(6) Subtraction with two literals" -: compileBlock [] emptyRA 0 [DeclareAndAssignStmt (Var IntType "x") (SubtractExpr (IntAtom 5) (IntAtom 3))] --> ([MOV L (Immediate 5) R13, MOV L (Immediate 3) R14, SUB L R13 R14 R15, MOV L R15 R12], 0)
            , "(7) Multiplication of literal and live variable" -: compileBlock [Var IntType "x"] (makeRA [(Var IntType "x", R12)]) 0 [DeclareAndAssignStmt (Var IntType "y") (MultiplyExpr (IntAtom 4) (VarAtom (Var IntType "x")))] --> ([MOV L (Immediate 4) R14, IMUL L R14 R12 R15, MOV L R15 R13], 0)
            , "(8) While loop with literal condition and empty body" -: 
                            let 
                                -- while (1) {}
                                expr = AtomExpr (IntAtom 1)
                                block = []
                                lvs = []
                                startLabelId = 0
                            in
                                compileStmt lvs emptyRA startLabelId (WhileStmt expr block) 
                                --> ( [ Label 0
                                    , MOV L (Immediate 1) R12
                                    , TEST L R12 R12
                                    , JE (Label 1)
                                    , JMP (Label 0)
                                    , Label 1
                                    ]
                                    , 2 )
            , "(9) While loop with live variable condition and assignment body" -: compileStmt [Var IntType "x"] (makeRA [(Var IntType "x", R12)]) 4 (WhileStmt (AtomExpr (VarAtom (Var IntType "x"))) [DeclareAndAssignStmt (Var IntType "y") (AtomExpr (IntAtom 2))]) --> ([Label 4, TEST L R12 R12, JE (Label 5), MOV L (Immediate 2) R14, MOV L R14 R13, JMP (Label 4), Label 5], 6)
            , "(10) Single ExprStmt with AssignExpr (x = 5;)" -: compileBlock [Var IntType "x"] (makeRA [(Var IntType "x", R12)]) 0 [ExprStmt (AssignExpr (Var IntType "x") (AtomExpr (IntAtom 5)))] --> ([MOV L (Immediate 5) R13, MOV L R13 R12], 0)
            , "(11) Assigning one variable to another (x = y;)" -: compileBlock [Var IntType "x", Var IntType "y"] (makeRA [(Var IntType "x", R12), (Var IntType "y", R13)]) 0 [ExprStmt (AssignExpr (Var IntType "x") (AtomExpr (VarAtom (Var IntType "y"))))] --> ([MOV L R13 R12], 0)
            , "(12) Chained assignments (x = y = 5;)" -: compileBlock [Var IntType "x", Var IntType "y"] (makeRA [(Var IntType "x", R12), (Var IntType "y", R13)]) 0 [ExprStmt (AssignExpr (Var IntType "x") (AssignExpr (Var IntType "y") (AtomExpr (IntAtom 5))))] --> ([MOV L (Immediate 5) R14, MOV L R14 R13, MOV L R13 R12], 0)
            , "(13) Assignment with arithmetic expression (x = y + 3;)" -: compileBlock [Var IntType "x", Var IntType "y"] (makeRA [(Var IntType "x", R12), (Var IntType "y", R13)]) 0 [ExprStmt (AssignExpr (Var IntType "x") (AddExpr (VarAtom (Var IntType "y")) (IntAtom 3)))] --> ([MOV L (Immediate 3) R14, ADD L R13 R14 R15, MOV L R15 R12], 0)
            , "(14) Return a literal (return 0;)" -: compileBlock [] emptyRA 0 [ReturnStmt (AtomExpr (IntAtom 0))] --> ([MOV L (Immediate 0) R12, MOV L R12 RAX, RET], 0)
            , "(15) Return a live variable (return x;)" -: compileBlock [Var IntType "x"] (makeRA [(Var IntType "x", R12)]) 0 [ReturnStmt (AtomExpr (VarAtom (Var IntType "x")))] --> ([MOV L R12 RAX, RET], 0)
            , "(16) Return an arithmetic expression (return x + 5;)" -: compileBlock [Var IntType "x"] (makeRA [(Var IntType "x", R12)]) 0 [ReturnStmt (AddExpr (VarAtom (Var IntType "x")) (IntAtom 5))] --> ([MOV L (Immediate 5) R13, ADD L R12 R13 R14, MOV L R14 RAX, RET], 0)
        ]
    ) 
