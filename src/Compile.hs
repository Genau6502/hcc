module Compile where

import Types
import Instructions
import Registers
import Data.List((\\))

compileFunctions :: [Function] -> [Instruction]
compileFunctions fs = compileFunctions' fs 0
    where
        compileFunctions' :: [Function] -> Int -> [Instruction]
        compileFunctions' [] _ = []
        compileFunctions' (f:fs) l = let
            (i, l') = compileFunction f l
            is = compileFunctions' fs l'
            in (i ++ is)

compileFunction :: Function -> Int -> ([Instruction], Int)
compileFunction f@(Function name args _ b) label = let
    (bodyIs, maxStackSize, label') = compileBlock args (functionRA f) label b
    alignedStack = if maxStackSize == 0 then 0 
                   else ((maxStackSize + 8 + 15) `div` 16) * 16 - 8
    pre = FuncLabel name : if alignedStack > 0 then [SUB Q (Immediate alignedStack) RSP RSP] else []
    is = pre ++ handleRet alignedStack bodyIs
    in (is, label')
{-
    This wiil use the existing register allocation and preserve it. Any variables in scope within the block are only in the scope of the block.
    Register allocation is done at the block level

    Returns: instructions, total stack size, label counter
-}
compileBlock :: LiveVariables -> RegisterAllocation -> Int -> Block -> ([Instruction], Int, Int)
compileBlock outerLvs outerRA i stmts = let (instructions, _, maxStackSize, label) = compileBlock' outerLvs outerRA i stmts in (instructions, maxStackSize, label)
    where
        compileBlock' :: LiveVariables -> RegisterAllocation -> Int -> Block -> ([Instruction], RegisterAllocation, Int, Int)
        compileBlock' lvs ra i (stmt:stmts) = let
            lvs1 = clearDeadVars (stmt:stmts) outerLvs lvs
            deadVars = lvs \\ lvs1
            ra1 = freeDeadVars ra deadVars
            (lvs2, ra2) = processStmt stmt lvs1 ra1
            (is, so, i') = compileStmt lvs2 ra2 i stmt
            (is', ra3, maxSo, i'') = compileBlock' lvs2 ra2 i' stmts
            in (is ++ is', ra3, max maxSo so, i'')
        compileBlock' _ ra i' _ = ([], ra, stackOffset ra, i')

-- Returns instructions, max stack offset, label counter
compileStmt :: LiveVariables -> RegisterAllocation -> Int -> Stmt -> ([Instruction], Int, Int)
compileStmt lvs ra i (DeclareAndAssignStmt v expr) = let
            (is, loc) = compileExpr lvs ra expr
            in (is <++> MOV (sizeOf (typeOfExpr lvs expr)) loc (locationOf v ra), stackOffset ra, i)
compileStmt lvs ra i (WhileStmt expr block) = let
            condLabel = Label i
            condSize = (sizeOf (typeOfExpr lvs expr))
            endLabel = Label (i+1)
            (cond, res) = compileExpr lvs ra expr
            (body, so, i') = compileBlock lvs ra (i+2) block
            -- If the condition evaluates to false (zero), then we jump to the end of the loop
            -- At the end of the block, we jump back to condition evaluation
            in ((condLabel : cond <++> (TEST condSize res res) <++> (JE endLabel) ++ body <++> (JMP condLabel) <++> endLabel), max so (stackOffset ra), i')
compileStmt lvs ra i (ExprStmt e) = let (is, loc) = compileExpr lvs ra e in (is, stackOffset ra, i)
compileStmt lvs ra i (ReturnStmt e) = let
    (is, loc) = compileExpr lvs ra e
    in (is <++> MOV (sizeOf (typeOfExpr lvs e)) loc RAX <++> RET_PLA, stackOffset ra, i)
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

handleRet :: Int -> [Instruction] -> [Instruction]
handleRet size = concatMap (replaceRet size)

replaceRet :: Int -> Instruction -> [Instruction]
replaceRet size RET_PLA = let spadd = ([ADD Q (Immediate size) RSP RSP | size > 0]) in spadd <++> RET
replaceRet _ i = [i]

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
