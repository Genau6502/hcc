module Compile where

import Types
import Instructions
import Registers
import TypeCheck
import ParseContext

{-
    This wiil use the existing register allocation and preserve it. Any variables in scope within the block are only in the scope of the block.
    Register allocation is done at the block level
-}
compileBlock :: LiveVariables -> RegisterAllocation -> [Stmt] -> [Instruction]
compileBlock outerLvs ra stmts = compileBlock' outerLvs stmts
    where
        blockra = allocateRegistersForBlock outerLvs ra stmts
        compileBlock' :: LiveVariables -> [Stmt] -> [Instruction]
        compileBlock' lvs (stmt:stmts) = let
            is = compileStmt lvs blockra stmt
            lvs' = addLiveVariables stmt lvs
            lvs'' = clearDeadVars stmts outerLvs lvs'
            in is ++ compileBlock' lvs'' stmts
        compileBlock' _ _ = []

addLiveVariables :: Stmt -> LiveVariables -> LiveVariables
addLiveVariables (DeclareAndAssignStmt v _) lvs = (v:lvs)

compileStmt :: LiveVariables -> RegisterAllocation -> Stmt -> [Instruction]
compileStmt lvs ra (DeclareAndAssignStmt v expr) = let
            (is, loc) = compileExpr lvs ra expr
            in is <**> MOV (sizeOf (typeOfExpr lvs expr)) loc (locationOf v ra)

compileAtom :: LiveVariables -> RegisterAllocation -> Atom -> ([Instruction], Location, LiveVariables, RegisterAllocation)
compileAtom lvs ra (VarAtom v) = ([], locationOf v ra, lvs, ra)
compileAtom lvs ra (IntAtom i) = let (loc, ra', lvs') = allocateDummyVar lvs ra in ([MOV_I (sizeOf IntType) i loc], loc, lvs', ra')

compileExpr :: LiveVariables -> RegisterAllocation -> Expr -> ([Instruction], Location)
compileExpr lvs ra (AddExpr a1 a2) = let
    (is1, loc1, lvs1, ra1) = compileAtom lvs ra a1
    (is2, loc2, lvs2, _) = compileAtom lvs1 ra1 a2
    in (is1 ++ is2 <**> (ADD (sizeOf (typeOfAtom lvs a1)) loc1 loc2 loc1), loc1)
compileExpr lvs ra (SubtractExpr a1 a2) = let
    (is1, loc1, lvs1, ra1) = compileAtom lvs ra a1
    (is2, loc2, lvs2, _) = compileAtom lvs1 ra1 a2
    in (is1 ++ is2 <**> (SUB (sizeOf (typeOfAtom lvs a1)) loc1 loc2 loc1), loc1)
--todo handle this signed vs unsigned
compileExpr lvs ra (MultiplyExpr a1 a2) = let
    (is1, loc1, lvs1, ra1) = compileAtom lvs ra a1
    (is2, loc2, lvs2, _) = compileAtom lvs1 ra1 a2
    in (is1 ++ is2 <**> (IMUL (sizeOf (typeOfAtom lvs a1)) loc1 loc2 loc1), loc1)
compileExpr lvs ra (AtomExpr a) = let (is, loc, _, _) = compileAtom lvs ra a in (is, loc)

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
