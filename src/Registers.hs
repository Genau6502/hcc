module Registers(allocateDummyVar, locationOf, LiveVariables, clearDeadVars, freeDeadVars, emptyRA, processStmt, functionRA) where

import Types

registers :: [Location]
-- This is in the order we want them to be used. Prioritise callee saved registers.
-- We do not include RSP as we never want to overwrite the stack pointer
registers = [R12, R13, R14, R15, RBX, RBP, R10, R11, RDI, RSI, RDX, RCX, R8, R9]

emptyRA :: RegisterAllocation
emptyRA = RegisterAllocation registers 0 []

functionRegisters :: [Location]
functionRegisters = [RDI, RSI, RDX, RCX, R8, R9]

--todo consider more than 6 args
functionRA :: Function -> RegisterAllocation
functionRA (Function name args t _) = RegisterAllocation registers 0 (zip args functionRegisters)

type LiveVariables = [Var]

freeDeadVar :: Var -> RegisterAllocation -> RegisterAllocation
freeDeadVar v ra = case lookup v (allocations ra) of
        (Just loc) ->
            let ra' = ra { allocations = filter ((/=v).fst) (allocations ra) } in
            case loc of
                Stack _ -> ra'
                Immediate _ -> ra'
                r -> ra' { freeregs = r:freeregs ra' }
        Nothing -> ra


processStmt :: Stmt -> LiveVariables -> RegisterAllocation -> (LiveVariables, RegisterAllocation)
processStmt (DeclareAndAssignStmt (Var t n) _) lvs ra = let
    (loc, ra') = allocateLocation t ra
    in (Var t n:lvs, ra' { allocations = (Var t n, loc):allocations ra})
processStmt (ExprStmt (AssignExpr _ _)) lvs ra = (lvs, ra)
processStmt _ lvs ra = (lvs, ra)

-- Pre: variable is live
locationOf :: Var -> RegisterAllocation -> Location
locationOf v ra = lookupUnsafe v (allocations ra)

allocateLocation :: Type -> RegisterAllocation -> (Location, RegisterAllocation)
--todo: consider that we can't have all mem arithmetic instructions-}
allocateLocation t ra = case freeregs ra of
    (r:rs) -> (r, ra { freeregs = rs })
    [] -> let
        sizeOnStack = sizeToBytes (sizeOf t)
        currentOffset = stackOffset ra
        in (Stack currentOffset, ra { stackOffset = currentOffset + sizeOnStack})



{- selectLocation registers
    where
        currentLocations :: [Location]
        currentLocations = map (\v -> lookupUnsafe v ra) lvs

        selectLocation :: [Location] -> Location
        selectLocation (l:ls) = if (any (==l) currentLocations) then selectLocation ls else l
        selectLocation [] = undefined 
-}
lookupUnsafe :: Eq a => a -> [(a, b)] -> b
lookupUnsafe x ((y, z):xs)
    | x == y = z
    | otherwise = lookupUnsafe x xs

clearDeadVars :: [Stmt] -> LiveVariables -> LiveVariables -> LiveVariables
clearDeadVars stmts outerLvs = filter (\v -> isVariableLive stmts v || elem v outerLvs)

freeDeadVars :: RegisterAllocation -> [Var] -> RegisterAllocation
freeDeadVars = foldr freeDeadVar

isVariableLive :: [Stmt] -> Var -> Bool
isVariableLive _ (DummyVar _ _) = False
isVariableLive stmts v = any stmtContainsVar stmts
    where
        stmtContainsVar :: Stmt -> Bool
        stmtContainsVar (DeclareAndAssignStmt _ e) = exprContainsVar e
        stmtContainsVar (ReturnStmt e) = exprContainsVar e
        stmtContainsVar (ExprStmt e) = exprContainsVar e
        stmtContainsVar (WhileStmt e b) = any stmtContainsVar b || exprContainsVar e

        exprContainsVar :: Expr -> Bool
        exprContainsVar (AddExpr a1 a2) = atomContainsVar a1 || atomContainsVar a2
        exprContainsVar (SubtractExpr a1 a2) = atomContainsVar a1 || atomContainsVar a2
        exprContainsVar (MultiplyExpr a1 a2) = atomContainsVar a1 || atomContainsVar a2
        exprContainsVar (AtomExpr a) = atomContainsVar a
        exprContainsVar (MinusExpr a) = atomContainsVar a
        exprContainsVar (AssignExpr v' e) = v == v' || exprContainsVar e

        atomContainsVar :: Atom -> Bool
        atomContainsVar (VarAtom v') = v==v'
        atomContainsVar (ParenAtom e) = exprContainsVar e
        atomContainsVar _ = False

{-
A dummy variable is one which is used for the compilation of a single statement, for example when evaluating expressions

TODO: do this properly
-}
allocateDummyVar :: Type -> LiveVariables -> RegisterAllocation -> (Location, RegisterAllocation, LiveVariables)
allocateDummyVar t lvs ra = let
    size = sizeOf t
    v = DummyVar size (length lvs)
    (loc, ra') = allocateLocation t ra
    in (loc, ra' { allocations = (v, loc) : allocations ra' }, v:lvs)

{-

ALGORITHM

contain allocation and live variables.

If a variable has no further occurences, we remove it from live variables.

We can then reuse that register for new allocations
-}