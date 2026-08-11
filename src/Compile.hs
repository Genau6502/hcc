module Compile where

import Types
import Instructions
import Registers
import TypeCheck
import ParseContext

compileStmt :: Context -> LiveVariables -> RegisterAllocation -> Stmt -> [Instruction]
compileStmt ctx lvs ra (DeclareAndAssignStmt v expr) = let
            (is, loc) = compileExpr ctx lvs ra expr
            in is <**> MOV (sizeOf (typeOfExpr ctx expr)) loc (locationOf v ra)

compileAtom :: Context -> LiveVariables -> RegisterAllocation -> Atom -> ([Instruction], Location)
compileAtom ctx _ ra (VarAtom v) = ([], locationOf v ra)
compileAtom ctx lvs ra (IntAtom i) = let (loc, ra') = allocateDummyVar lvs ra in ([MOV_I (sizeOf IntType) i loc], loc)

compileExpr :: Context -> LiveVariables -> RegisterAllocation -> Expr -> ([Instruction], Location)
compileExpr ctx lvs ra (AddExpr a1 a2) = let
    (is1, loc1) = compileAtom ctx lvs ra a1
    (is2, loc2) = compileAtom ctx lvs ra a2
    in (is1 ++ is2 <**> (ADD_I (sizeOf (typeOfAtom ctx a1)) loc1 loc2 loc1), loc2)

(<**>) :: [a] -> a -> [a]
xs <**> x = xs ++ [x]
