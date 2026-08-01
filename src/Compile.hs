module Compile(compileBlock, compileStmt) where

import Types

compileStmt :: LiveVariables -> RegisterAllocation -> Stmt -> Instruction
compileStmt ra stmt = undefined

compileExprResult :: LiveVariables -> RegisterAllocation -> Expr -> Location
compilation lvs ra expr
    where
        dummy = allocateDummyVar lvs ra


exprToInstruction
