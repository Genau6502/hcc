module CompileTest where

import TestTypes
import Types
import Registers
import Compile 
import Instructions 
import Data.List((\\))

makeRA :: [(Var, Location)] -> RegisterAllocation
makeRA allocs = emptyRA { allocations = allocs, freeregs = freeregs emptyRA \\ map snd allocs}

compileBlockTests :: TestGroup
compileBlockTests = TestGroup ("Compile tests", [   
      "(1) Empty block compilation" -: compileBlock [] emptyRA 0 [] --> ([], 0, 0)
    , "(2) Single declare and assign statement" -: compileBlock [] emptyRA 0 [DeclareAndAssignStmt (Var IntType "x") (AtomExpr (IntAtom 0))] --> ([MOV L (Immediate 0) R13,MOV L R13 R12], 0, 0)
    , "(3) Sequential statements with live variable carried over" -: compileBlock [] emptyRA 0 [DeclareAndAssignStmt (Var IntType "x") (AtomExpr (IntAtom 2)), DeclareAndAssignStmt (Var IntType "y") (AtomExpr (VarAtom (Var IntType "x")))] --> ([MOV L (Immediate 2) R13, MOV L R13 R12,MOV L R12 R13], 0, 0)
    , "(4) Sequential statements where a variable dies immediately" -: compileBlock [Var IntType "x", Var IntType "y", Var IntType "z"] emptyRA 0 [DeclareAndAssignStmt (Var IntType "x") (AtomExpr (IntAtom 2)), DeclareAndAssignStmt (Var IntType "y") (AtomExpr (IntAtom 3)), DeclareAndAssignStmt (Var IntType "z") (AtomExpr (VarAtom (Var IntType "y")))] --> ([MOV L (Immediate 2) R13, MOV L R13 R12, MOV L (Immediate 3) R14, MOV L R14 R13, MOV L R13 R14], 0, 0)
    , "(5) Statement requiring two dummy variables (binary expression)" -: compileBlock [] emptyRA 0 [DeclareAndAssignStmt (Var IntType "z") (AddExpr (IntAtom 2) (IntAtom 3))] --> ([MOV L (Immediate 2) R13, MOV L (Immediate 3) R15, MOV L R13 R14, ADD L R15 R14, MOV L R14 R12], 0, 0)
    , "(6) Subtraction with two literals" -: compileBlock [] emptyRA 0 [DeclareAndAssignStmt (Var IntType "x") (SubtractExpr (IntAtom 5) (IntAtom 3))] --> ([MOV L (Immediate 5) R13, MOV L (Immediate 3) R15, MOV L R13 R14, SUB L R15 R14, MOV L R14 R12], 0, 0)
    , "(7) Multiplication of literal and live variable" -: compileBlock [Var IntType "x"] (makeRA [(Var IntType "x", R12)]) 0 [DeclareAndAssignStmt (Var IntType "y") (MultiplyExpr (IntAtom 4) (VarAtom (Var IntType "x")))] --> ([MOV L (Immediate 4) R14, MOV L R14 R15, IMUL L R12 R15, MOV L R15 R13], 0, 0)
    , "(8) While loop with literal condition and empty body" -: compileStmt [] emptyRA 0 (WhileStmt (AtomExpr (IntAtom 1)) []) --> ([Label 0, MOV L (Immediate 1) R12, TEST L R12 R12, JE (Label 1), JMP (Label 0), Label 1], 0, 2)
    , "(9) While loop with live variable condition and assignment body" -: compileStmt [Var IntType "x"] (makeRA [(Var IntType "x", R12)]) 4 (WhileStmt (AtomExpr (VarAtom (Var IntType "x"))) [DeclareAndAssignStmt (Var IntType "y") (AtomExpr (IntAtom 2))]) --> ([Label 4, TEST L R12 R12, JE (Label 5), MOV L (Immediate 2) R14, MOV L R14 R13, JMP (Label 4), Label 5], 0, 6)
    , "(10) Single ExprStmt with AssignExpr (x = 5;)" -: compileBlock [Var IntType "x"] (makeRA [(Var IntType "x", R12)]) 0 [ExprStmt (AssignExpr (Var IntType "x") (AtomExpr (IntAtom 5)))] --> ([MOV L (Immediate 5) R13, MOV L R13 R12], 0, 0)
    , "(11) Assigning one variable to another (x = y;)" -: compileBlock [Var IntType "x", Var IntType "y"] (makeRA [(Var IntType "x", R12), (Var IntType "y", R13)]) 0 [ExprStmt (AssignExpr (Var IntType "x") (AtomExpr (VarAtom (Var IntType "y"))))] --> ([MOV L R13 R12], 0, 0)
    , "(12) Chained assignments (x = y = 5;)" -: compileBlock [Var IntType "x", Var IntType "y"] (makeRA [(Var IntType "x", R12), (Var IntType "y", R13)]) 0 [ExprStmt (AssignExpr (Var IntType "x") (AssignExpr (Var IntType "y") (AtomExpr (IntAtom 5))))] --> ([MOV L (Immediate 5) R14, MOV L R14 R13, MOV L R13 R12], 0, 0)
    , "(13) Assignment with arithmetic expression (x = y + 3;)" -: compileBlock [Var IntType "x", Var IntType "y"] (makeRA [(Var IntType "x", R12), (Var IntType "y", R13)]) 0 [ExprStmt (AssignExpr (Var IntType "x") (AddExpr (VarAtom (Var IntType "y")) (IntAtom 3)))] --> ([MOV L (Immediate 3) R15, MOV L R13 R14, ADD L R15 R14, MOV L R14 R12], 0, 0)
    , "(14) Return a literal (return 0;)" -: compileBlock [] emptyRA 0 [ReturnStmt (AtomExpr (IntAtom 0))] --> ([MOV L (Immediate 0) R12, MOV L R12 RAX, RET_PLA], 0, 0)
    , "(15) Return a live variable (return x;)" -: compileBlock [Var IntType "x"] (makeRA [(Var IntType "x", R12)]) 0 [ReturnStmt (AtomExpr (VarAtom (Var IntType "x")))] --> ([MOV L R12 RAX, RET_PLA], 0, 0)
    , "(16) Return an arithmetic expression (return x + 5;)" -: compileBlock [Var IntType "x"] (makeRA [(Var IntType "x", R12)]) 0 [ReturnStmt (AddExpr (VarAtom (Var IntType "x")) (IntAtom 5))] --> ([MOV L (Immediate 5) R14, MOV L R12 R13, ADD L R14 R13, MOV L R13 RAX, RET_PLA], 0, 0)
    , "(17) Assignment with parentheses and subtraction" -: compileBlock [Var IntType "y", Var IntType "x"] emptyRA 0 [DeclareAndAssignStmt (Var IntType "y") (AtomExpr (IntAtom 1)), DeclareAndAssignStmt (Var IntType "x") (SubtractExpr (ParenAtom (AddExpr (VarAtom (Var IntType "y")) (IntAtom 1))) (IntAtom 2))] --> ([MOV L (Immediate 1) R13, MOV L R13 R12, MOV L (Immediate 1) R15, MOV L R12 R14, ADD L R15 R14, MOV L (Immediate 2) RBP, MOV L R14 RBX, SUB L RBP RBX, MOV L RBX R13], 0, 0)
    ])

compileFunctionTests :: TestGroup
compileFunctionTests = TestGroup ("Compile Function tests", [   
      "(1) Simple main function returning literal: int main() { return 1; }" -: compileFunction (Function "main" [] IntType [ReturnStmt (AtomExpr (IntAtom 1))]) 0 --> ([FuncLabel "main", MOV L (Immediate 1) R12, MOV L R12 RAX, RET], 0)
    , "(2) Function with 1 argument: int identity(int x) { return x; }" -: compileFunction (Function "identity" [Var IntType "x"] IntType [ReturnStmt (AtomExpr (VarAtom (Var IntType "x")))]) 0 --> ([FuncLabel "identity", MOV L RDI RAX, RET], 0)
    , "(3) Function with 2 arguments and arithmetic: int add(int a, int b) { return a + b; }" -: compileFunction (Function "add" [Var IntType "a", Var IntType "b"] IntType [ReturnStmt (AddExpr (VarAtom (Var IntType "a")) (VarAtom (Var IntType "b")))]) 0 --> ([FuncLabel "add", MOV L RDI R12, ADD L RSI R12, MOV L R12 RAX, RET], 0)
    , "(4) Function with empty body (compiles cleanly without crashing)" -: compileFunction (Function "empty" [] VoidType []) 0 --> ([FuncLabel "empty"], 0)
    ])
