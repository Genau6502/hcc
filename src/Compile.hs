module Compile(compileLine) where

import Types

compileStmt :: RegisterAllocation -> Stmt -> Instruction
compileStmt