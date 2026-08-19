module ParseContext(emptyContext, putVarInScope, removeVarFromScope, varInScope, putVarsInScope, Context, checkFunctionReturnType, varContext, funcContext, funcInScope) where

import Types

-- Represents: return type, variables in scope
data Context = ParseContext Type [Function] [Var]
    deriving (Eq, Show)

-- For use in testing
varContext :: [Var] -> Context
varContext = ParseContext VoidType []

funcContext :: Type -> [Function] -> [Var] -> Context
funcContext t = ParseContext t

emptyContext :: Context
emptyContext = ParseContext VoidType [] []

checkFunctionReturnType :: Context -> Type -> Either Error Type
checkFunctionReturnType (ParseContext t fs _) t'
    | t == t' = Right t
    | otherwise = Left $ ReturnTypeMismatch t t'

putVarsInScope :: Context -> [Var] -> Context
putVarsInScope (ParseContext t fs vs) vars = ParseContext t fs (vs ++ vars)

putVarInScope :: Context -> Var -> Context
putVarInScope (ParseContext t fs vs) v = ParseContext t fs (v:vs)

--todo: consider if we need to handle errors in this
removeVarFromScope :: Context -> Var -> Context
removeVarFromScope (ParseContext t fs vs) var = ParseContext t fs (filter (/=var) vs)

varInScope :: Context -> String -> Maybe Var
varInScope (ParseContext t fs ((Var t' n):cs)) name
    | n == name = Just (Var t' n)
    | otherwise = varInScope (ParseContext t fs cs) name
varInScope _ _ = Nothing

funcInScope :: Context -> String -> Maybe Function
funcInScope (ParseContext t (f@(Function n _ _ _):fs) cs) name
    | n == name = Just f
    | otherwise = funcInScope (ParseContext t fs cs) name
funcInScope _ _ = Nothing

errorIfAlreadyContainsVarName :: Context -> String -> Either Error Context
errorIfAlreadyContainsVarName (ParseContext t fs vars) name
    | any (\(Var _ n) -> n == name) vars = Left $ VariableAlreadyDeclared name
    | otherwise = pure (ParseContext t fs vars)
