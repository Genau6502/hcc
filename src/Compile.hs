module Compile where

import Types
import Instructions
import Registers
import TypeCheck
import ParseContext

{-
    This wiil use the existing register allocation and preserve it. Any variables in scope within the block are only in the scope of the block.
-}
compileBlock :: LiveVariables -> RegisterAllocation -> [Stmt] -> [Instruction]
compileBlock lvs ra (stmt:stmts) = let
    is = compileStmt lvs ra stmt
    lvs' = clearDeadVars stmts lvs
    in is ++ compileBlock lvs' ra stmts
compileBlock lvs ra _ = []

compileStmt :: LiveVariables -> RegisterAllocation -> Stmt -> [Instruction]
compileStmt lvs ra (DeclareAndAssignStmt v expr) = let
            (is, loc) = compileExpr lvs ra expr
            in is <**> MOV (sizeOf (typeOfExpr lvs expr)) loc (locationOf v ra)

compileAtom :: LiveVariables -> RegisterAllocation -> Atom -> ([Instruction], Location)
compileAtom _ ra (VarAtom v) = ([], locationOf v ra)
compileAtom lvs ra (IntAtom i) = let (loc, ra') = allocateDummyVar lvs ra in ([MOV_I (sizeOf IntType) i loc], loc)

compileExpr :: LiveVariables -> RegisterAllocation -> Expr -> ([Instruction], Location)
compileExpr lvs ra (AddExpr a1 a2) = let
    (is1, loc1) = compileAtom lvs ra a1
    (is2, loc2) = compileAtom lvs ra a2
    in (is1 ++ is2 <**> (ADD_I (sizeOf (typeOfAtom lvs a1)) loc1 loc2 loc1), loc2)
compileExpr lvs ra (AtomExpr a) = compileAtom lvs ra a

typeOfAtom :: LiveVariables -> Atom -> Type
typeOfAtom _ (IntAtom _) = IntType
typeOfAtom _ (VarAtom (Var t _)) = t
typeOfAtom _ (CharAtom _) = CharType
typeOfAtom lvs (ParenAtom e) = typeOfExpr lvs e
typeOfAtom _ (CastAtom t e) = t

typeOfExpr :: LiveVariables -> Expr -> Type
typeOfExpr lvs (AddExpr x y) = typeOfAtom lvs x
typeOfExpr lvs (SubtractExpr x y) = typeOfAtom lvs x
typeOfExpr lvs (MultiplyExpr x y) = typeOfAtom lvs x
typeOfExpr lvs (AtomExpr x) = typeOfAtom lvs x

(<**>) :: [a] -> a -> [a]
xs <**> x = xs ++ [x]
