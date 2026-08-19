module ParseContext(emptyContext, putVarInScope, removeVarFromScope, varInScope, putVarsInScope, Context, checkFunctionReturnType, varContext, funcContext) where

import Types

-- Represents: return type, variables in scope
data Context = ParseContext Type [Var]
    deriving (Eq, Show)

-- For use in testing
varContext :: [Var] -> Context
varContext = ParseContext VoidType

funcContext :: Type -> [Var] -> Context
funcContext = ParseContext

emptyContext :: Context
emptyContext = ParseContext VoidType []

checkFunctionReturnType :: Context -> Type -> Either Error Type
checkFunctionReturnType (ParseContext t _) t'
    | t == t' = Right t
    | otherwise = Left $ ReturnTypeMismatch t t'

putVarsInScope :: Context -> [Var] -> Context
putVarsInScope (ParseContext t vs) vars = ParseContext t (vs ++ vars)

putVarInScope :: Context -> Var -> Context
putVarInScope (ParseContext t vs) v = ParseContext t (v:vs)

--todo: consider if we need to handle errors in this
removeVarFromScope :: Context -> Var -> Context
removeVarFromScope (ParseContext t vs) var = ParseContext t (filter (/=var) vs)

varInScope :: Context -> String -> Maybe Var
varInScope (ParseContext t ((Var t' n):cs)) name
    | n == name = Just (Var t' n)
    | otherwise = varInScope (ParseContext t cs) name
varInScope _ _ = Nothing

errorIfAlreadyContainsVarName :: Context -> String -> Either Error Context
errorIfAlreadyContainsVarName (ParseContext t vars) name
    | any (\(Var _ n) -> n == name) vars = Left $ VariableAlreadyDeclared name
    | otherwise = pure (ParseContext t vars)
