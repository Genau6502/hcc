module ParseContext(emptyContext, putVarInScope, removeVarFromScope, varInScope, putVarsInScope, Context) where

import Types

type Context = [Var]

putVarsInScope :: Context -> [Var] -> Context
putVarsInScope = foldl putVarInScope

putVarInScope :: Context -> Var -> Context
putVarInScope cs var = var:cs

--todo: consider if we need to handle errors in this
removeVarFromScope :: Context -> Var -> Context
removeVarFromScope cs var = filter (/=var) cs

varInScope :: Context -> String -> Maybe Var
varInScope ((Var t n):cs) name
    | n == name = Just (Var t n)
    | otherwise = varInScope cs name
varInScope _ _ = Nothing

errorIfAlreadyContainsVarName :: Context -> String -> Either Error Context
errorIfAlreadyContainsVarName ((Var t n):cs) name
    | n == name = Left $ VariableAlreadyDeclared name
    | otherwise = (:) (Var t n) <$> errorIfAlreadyContainsVarName cs name
errorIfAlreadyContainsVarName [] _ = Right []

emptyContext :: Context
emptyContext = []
