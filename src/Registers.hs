module Registers(allocateRegistersForBlock, allocateDummyVar, locationOf, LiveVariables, clearDeadVars) where

import Data.Maybe
import Types

registers :: [Location]
registers = [R12, R13, R14, R15, DummyReg 1, DummyReg 2]

type LiveVariables = [Var]

{-
    Registers are allocated as follows
    For each block, we hand the existing register allocation to this function.
    We then add 


    PROCESS A BLOCK AS FOLLOWS
    - Take the preceeding live variables and register allocation.
    - Compute the register allocations for the block providing the existing register allocation

    The effect is that register allocations are scoped to the block they are actually in.
-}

--todo make this more sensible - use internal state to tell when variables go in and out of scope
allocateRegistersForBlock :: LiveVariables -> RegisterAllocation -> [Stmt] -> RegisterAllocation
allocateRegistersForBlock = allocateRegisters'
    where
        allocateRegisters' :: LiveVariables -> RegisterAllocation -> [Stmt] -> RegisterAllocation
        allocateRegisters' lvs ra [] = ra
        allocateRegisters' lvs ra (s:stmts)
            = let
                (lvs', ra') = processStmt s lvs ra
                lvs'' = clearDeadVars stmts lvs'
                in allocateRegisters' lvs'' ra' stmts

processStmt :: Stmt -> LiveVariables -> RegisterAllocation -> (LiveVariables, RegisterAllocation)
processStmt (DeclareAndAssignStmt v _) lvs ra = (v:lvs, ((v, allocateRegister lvs ra):ra))

-- Pre: variable is live
locationOf :: Var -> RegisterAllocation -> Location
locationOf ra v = let (Just x) = lookup ra v in x

allocateRegister :: LiveVariables -> RegisterAllocation -> Location
allocateRegister lvs ra = selectLocation registers
    where
        currentLocations :: [Location]
        currentLocations = map (\v -> lookupUnsafe v ra) lvs
        selectLocation :: [Location] -> Location
        selectLocation (l:ls) = if (any (==l) currentLocations) then selectLocation ls else l

lookupUnsafe :: Eq a => a -> [(a, b)] -> b
lookupUnsafe x ((y, z):xs)
    | x == y = z
    | otherwise = lookupUnsafe x xs

clearDeadVars :: [Stmt] -> LiveVariables -> LiveVariables
clearDeadVars stmts = filter (not.(isVariableLive stmts))

isVariableLive :: [Stmt] -> Var -> Bool
isVariableLive _ (DummyVar _) = False
isVariableLive stmts v = any stmtContainsVar stmts
    where
        stmtContainsVar :: Stmt -> Bool
        stmtContainsVar (DeclareAndAssignStmt _ e) = exprContainsVar e
        stmtContainsVar (ReturnStmt e) = exprContainsVar e

        exprContainsVar :: Expr -> Bool
        exprContainsVar (AddExpr a1 a2) = atomContainsVar a1 || atomContainsVar a2
        exprContainsVar (SubtractExpr a1 a2) = atomContainsVar a1 || atomContainsVar a2
        exprContainsVar (MultiplyExpr a1 a2) = atomContainsVar a1 || atomContainsVar a2
        exprContainsVar (AtomExpr a) = atomContainsVar a
        exprContainsVar (MinusExpr a) = atomContainsVar a

        atomContainsVar :: Atom -> Bool
        atomContainsVar (VarAtom v') = v==v'
        atomContainsVar _ = False

{-
A dummy variable is one which is used for the compilation of a single statement, for example when evaluating expressions

TODO: do this properly
-}
allocateDummyVar :: LiveVariables -> RegisterAllocation -> (Location, RegisterAllocation, LiveVariables)
allocateDummyVar lvs ra = let
                            v = DummyVar (length lvs)
                            loc = allocateRegister lvs ra
                            in (allocateRegister lvs ra, (v, loc):ra, v:lvs)

{-

ALGORITHM

contain allocation and live variables.

If a variable has no further occurences, we remove it from live variables.

We can then reuse that register for new allocations
-}