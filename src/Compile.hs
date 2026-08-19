module Compile where

import Types
import Instructions
import Registers
import TypeCheck
import Data.List((\\))

{-
    This wiil use the existing register allocation and preserve it. Any variables in scope within the block are only in the scope of the block.
    Register allocation is done at the block level
-}
compileBlock :: LiveVariables -> RegisterAllocation -> Int -> Block -> ([Instruction], Int)
compileBlock outerLvs outerRA i stmts = let (instructions, _, label) = compileBlock' outerLvs outerRA i stmts in (instructions, label)
    where
        compileBlock' :: LiveVariables -> RegisterAllocation -> Int -> Block -> ([Instruction], RegisterAllocation, Int)
        compileBlock' lvs ra i (stmt:stmts) = let
            lvs1 = clearDeadVars (stmt:stmts) outerLvs lvs
            deadVars = lvs \\ lvs1
            ra1 = freeDeadVars ra deadVars
            (lvs2, ra2) = processStmt stmt lvs1 ra1
            (is, i') = compileStmt lvs2 ra2 i stmt
            (is', ra3, i'') = compileBlock' lvs2 ra2 i' stmts
            in (is ++ is', ra3, i'')
        compileBlock' _ ra i' _ = ([], ra, i')

compileStmt :: LiveVariables -> RegisterAllocation -> Int -> Stmt -> ([Instruction], Int)
compileStmt lvs ra i (DeclareAndAssignStmt v expr) = let
            (is, loc) = compileExpr lvs ra expr
            in (is <++> MOV (sizeOf (typeOfExpr lvs expr)) loc (locationOf v ra), i)
compileStmt lvs ra i (WhileStmt expr block) = let
            condLabel = Label i
            condSize = (sizeOf (typeOfExpr lvs expr))
            endLabel = Label (i+1)
            (cond, res) = compileExpr lvs ra expr
            (body, i') = compileBlock lvs ra (i+2) block

            -- If the condition evaluates to false (zero), then we jump to the end of the loop
            -- At the end of the block, we jump back to condition evaluation
            in ((condLabel : cond <++> (TEST condSize res res) <++> (JE endLabel) ++ body <++> (JMP condLabel) <++> endLabel), i')
compileStmt lvs ra i (ExprStmt e) = const i <$> compileExpr lvs ra e
compileStmt _ _ _ _ = undefined

compileAtom :: LiveVariables -> RegisterAllocation -> Atom -> ([Instruction], Location, LiveVariables, RegisterAllocation)
compileAtom lvs ra (VarAtom v) = ([], locationOf v ra, lvs, ra)
compileAtom lvs ra (IntAtom i) = let (loc, ra', lvs') = allocateDummyVar IntType lvs ra in ([MOV (sizeOf IntType) (Immediate i) loc], loc, lvs', ra')
compileAtom _ _ _ = undefined

compileExpr :: LiveVariables -> RegisterAllocation -> Expr -> ([Instruction], Location)
compileExpr lvs ra (AddExpr a1 a2) = let
    (is1, loc1, lvs1, ra1) = compileAtom lvs ra a1
    (is2, loc2, lvs2, ra2) = compileAtom lvs1 ra1 a2
    (dest, _, _) = allocateDummyVar (typeOfAtom lvs a1) lvs2 ra2
    in (is1 ++ is2 <++> (ADD (sizeOf (typeOfAtom lvs a1)) loc1 loc2 dest), dest)
compileExpr lvs ra (SubtractExpr a1 a2) = let
    (is1, loc1, lvs1, ra1) = compileAtom lvs ra a1
    (is2, loc2, lvs2, ra2) = compileAtom lvs1 ra1 a2
    (dest, _, _) = allocateDummyVar (typeOfAtom lvs a1) lvs2 ra2
    in (is1 ++ is2 <++> (SUB (sizeOf (typeOfAtom lvs a1)) loc1 loc2 dest), dest)
--todo handle this signed vs unsigned
compileExpr lvs ra (MultiplyExpr a1 a2) = let
    (is1, loc1, lvs1, ra1) = compileAtom lvs ra a1
    (is2, loc2, lvs2, ra2) = compileAtom lvs1 ra1 a2
    (dest, _, _) = allocateDummyVar (typeOfAtom lvs a1) lvs2 ra2
    in (is1 ++ is2 <++> (IMUL (sizeOf (typeOfAtom lvs a1)) loc1 loc2 dest), dest)
compileExpr lvs ra (AtomExpr a) = let (is, loc, _, _) = compileAtom lvs ra a in (is, loc)
compileExpr lvs ra (AssignExpr v e) = let
    (is, res) = compileExpr lvs ra e
    size = sizeOf (typeOfExpr lvs e)
    loc = locationOf v ra
    in (is <++> (MOV size res loc), loc)
compileExpr _ _ _ = undefined

typeOfAtom :: LiveVariables -> Atom -> Type
typeOfAtom _ (IntAtom _) = IntType
typeOfAtom _ (VarAtom (Var t _)) = t
typeOfAtom _ (CharAtom _) = CharType
typeOfAtom lvs (ParenAtom e) = typeOfExpr lvs e
typeOfAtom _ (CastAtom t e) = t
typeOfAtom _ (VarAtom _) = undefined

typeOfExpr :: LiveVariables -> Expr -> Type
typeOfExpr lvs (AddExpr x y) = typeOfAtom lvs x
typeOfExpr lvs (SubtractExpr x y) = typeOfAtom lvs x
typeOfExpr lvs (MultiplyExpr x y) = typeOfAtom lvs x
typeOfExpr lvs (AtomExpr x) = typeOfAtom lvs x
typeOfExpr lvs (AssignExpr v e) = typeOfExpr lvs e

(<++>) :: [a] -> a -> [a]
xs <++> x = xs ++ [x]
