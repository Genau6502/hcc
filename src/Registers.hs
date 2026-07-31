module Registers(allocateRegisters) where

import Data.Maybe
import Types

registers :: [Location]
registers = [R12, R13, R14, R15]

type LiveVariables = [Var]

--todo make this more sensible - use internal state to tell when variables go in and out of scope
allocateRegisters :: [Stmt] -> RegisterAllocation
allocateRegisters = allocateRegisters' [] []
    where
        allocateRegisters' :: LiveVariables -> RegisterAllocation -> [Stmt] -> RegisterAllocation
        allocateRegisters' lvs ra [] = ra
        allocateRegisters' lvs ra (s:stmts) = allocateRegisters' lvs'' ra' stmts
            where
                (lvs', ra') = processStmt s lvs ra
                lvs'' = clearDeadVars stmts lvs'

processStmt :: Stmt -> LiveVariables -> RegisterAllocation -> (LiveVariables, RegisterAllocation)
processStmt (DeclareAndAssignStmt v _) lvs ra = (v:lvs, ((v, allocateRegister lvs ra):ra))

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
isVariableLive stmts (Var _ v) = any stmtContainsVar stmts
    where
        stmtContainsVar :: Stmt -> Bool
        stmtContainsVar (DeclareAndAssignStmt _ e) = exprContainsVar e
        stmtContainsVar (ReturnStmt e) = exprContainsVar e

        exprContainsVar :: Expr -> Bool
        exprContainsVar (AddExpr a1 a2) = atomContainsVar a1 || atomContainsVar a2
        exprContainsVar (SubtractExpr a1 a2) = atomContainsVar a1 || atomContainsVar a2
        exprContainsVar (MultiplyExpr a1 a2) = atomContainsVar a1 || atomContainsVar a2
        exprContainsVar (AtomExpr a) = atomContainsVar a

        atomContainsVar :: Atom -> Bool
        atomContainsVar (ValAtom v') = v==v'
        atomContainsVar _ = False

{-

ALGORITHM

contain allocation and live variables.

If a variable has no further occurences, we remove it from live variables.

We can then reuse that register for new allocations
-}