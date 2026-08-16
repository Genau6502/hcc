module Parser(parseAtom, parseExpr, parseStmt, parseBlock, parseDeclarator, parseAbstractDeclarator) where

import Types
import ParseContext
import TypeCheck

type Parser a = [Token] -> Either Error (a, [Token])

(<|>) :: Either a b -> Either a b -> Either a b
Right x <|> _ = Right x
Left _ <|> Right y = Right y
Left x <|> _ = Left x

parseBaseType :: String -> Either Error Type
parseBaseType "int" = pure IntType
parseBaseType _ = Left Unexpected

parseDeclarator :: Context -> Parser (String, Type)
parseDeclarator ctx (NatTok x : toks) = do
    t <- parseBaseType x
    ((name, f), toks') <- parseDeclarator' toks
    return ((name, f t), toks')
    where
        -- the type passed is the "base" type
        parseDeclarator' :: Parser (String, Type -> Type)
        -- This is ONLY for grouping parenthesis, not for function pointers
        parseDeclarator' (LParenTok:toks) = do
            ((name, inner), toks') <- parseDeclarator' toks
            toks'' <- consumeTok RParenTok toks'
            (outer, toks''') <- parsePost toks''
            return $ ((name, (\base -> (inner.outer) base)), toks''')
        parseDeclarator' (AsteriskTok:toks) = do
            ((name, inner), toks') <- parseDeclarator' toks
            return $ ((name, (\base -> inner (PointerType base))), toks')
        parseDeclarator' (NatTok x:toks) = do
            (f, toks') <- parsePost toks
            return ((x, f), toks')
        -- No more typing to declare
        parseDeclarator' toks = Left $ InvalidType toks
        
        parsePost :: Parser (Type -> Type)
        parsePost (LSqParenTok:toks) = do
            (e, toks') <- parseExpr ctx toks
            sizeExpr <- parseIntExpr ctx toks' e
            toks'' <- consumeTok RSqParenTok toks'
            (outer, toks''') <- parsePost toks''
            return $ ((\base -> ArrayType (outer base) sizeExpr), toks''')
        parsePost (LParenTok:toks) = do
            (argts, toks') <- parseArgTypes ctx toks
            (outer, toks'') <- parsePost toks'
            return $ ((\base -> FunctionType argts (outer base)), toks'')
        parsePost toks = pure (id, toks)
parseDeclarator _ toks = Left $ InvalidType toks

parseAbstractDeclarator :: Context -> Parser Type
parseAbstractDeclarator ctx (NatTok x : toks) = do
    t <- parseBaseType x
    (f, toks') <- parseAbstractDeclarator' toks
    return (f t, toks')
    where
        -- the type passed is the "base" type
        parseAbstractDeclarator' :: Parser (Type -> Type)
        parseAbstractDeclarator' (LParenTok : NatTok x : toks) = parsePost (LParenTok : NatTok x : toks)
        parseAbstractDeclarator' (LParenTok : RParenTok : toks) = parsePost (LParenTok : RParenTok : toks)
        -- This is ONLY for grouping parenthesis, not for function pointers
        parseAbstractDeclarator' (LParenTok:toks) = do
            (inner, toks') <- parseAbstractDeclarator' toks
            toks'' <- consumeTok RParenTok toks'
            (outer, toks''') <- parsePost toks''
            -- Note: expanded (inner.outer) to inner(outer base) for safety
            return $ ((\base -> inner (outer base)), toks''')
        parseAbstractDeclarator' (AsteriskTok:toks) = do
            (inner, toks') <- parseAbstractDeclarator' toks
            return $ ((\base -> inner (PointerType base)), toks')
        -- No more typing to declare
        parseAbstractDeclarator' toks = parsePost toks
      
        parsePost :: Parser (Type -> Type)
        parsePost (LSqParenTok:toks) = do
            (e, toks') <- parseExpr ctx toks
            sizeExpr <- parseIntExpr ctx toks' e
            toks'' <- consumeTok RSqParenTok toks'
            (outer, toks''') <- parsePost toks''
            return $ ((\base -> ArrayType (outer base) sizeExpr), toks''')
        parsePost (LParenTok:toks) = do
            (argts, toks') <- parseArgTypes ctx toks
            (outer, toks'') <- parsePost toks'
            return $ ((\base -> FunctionType argts (outer base)), toks'')
        parsePost toks = pure (id, toks)
parseAbstractDeclarator _ toks = Left $ InvalidType toks

parseIntExpr :: Context -> [Token] -> Expr -> Either Error Expr
parseIntExpr ctx toks e = case (parseTypeOfExpr ctx e) of
    Left err -> Left err
    Right IntType -> Right e
    Right t -> Left $ InvalidType (NatTok "var_name" : toks)

parseArgTypes :: Context -> Parser [Type]
parseArgTypes ctx (RParenTok:toks) = pure ([], toks)
parseArgTypes ctx (NatTok x:toks) = do
    (t, toks') <- parseAbstractDeclarator ctx (NatTok x:toks)
    (ts, toks'') <- parseNextArgType ctx toks'
    return (t:ts, toks'')
parseArgTypes _ _ = Left $ ExpectedChar RParenTok

parseNextArgType :: Context -> Parser [Type]
parseNextArgType _ (RParenTok:toks) = pure ([], toks)
parseNextArgType ctx (CommaTok:toks) = do
    (ts, toks') <- parseArgTypes ctx toks
    return (ts, toks')
parseNextArgType _ _ = Left $ Unexpected  

parseBlock :: Context -> Parser ([Stmt], Context)
parseBlock vars toks = do
    toks' <- consumeTok LBraceTok toks
    parseNextStmt vars toks'
    where
        parseNextStmt :: [Var] -> Parser ([Stmt], Context)
        parseNextStmt ctx1 toks1 = ((\toks2 -> (([], ctx1), toks2)) <$> consumeTok RBraceTok toks1) <|> do
            ((stmt, ctx2), toks2) <- parseStmt ctx1 toks1
            ((stmts, ctx3), toks3) <- parseNextStmt ctx2 toks2
            return ((stmt:stmts, ctx3), toks3)

parseStmt :: Context -> Parser (Stmt, Context)
parseStmt ctx toks = parseWhileLoop <|> parseLineStmt
    where
        parseLineStmt :: Either Error ((Stmt, Context), [Token])
        parseLineStmt = do  (stmtctx, toks') <- parseDeclareAndAssignStmt <|> parseReturnStmt <|> parseExprStmt
                            (toks'') <- consumeTok SemiColonTok toks'
                            return (stmtctx, toks'')

        parseExprStmt :: Either Error ((Stmt, Context), [Token])
        parseExprStmt = do
            (expr, toks') <- parseExpr ctx toks
            return ((ExprStmt expr, ctx), toks')

        parseDeclareAndAssignStmt :: Either Error ((Stmt, Context), [Token])
        parseDeclareAndAssignStmt = do
            ((name, t), toks1) <- parseDeclarator ctx toks
            toks2 <- consumeTok EqualsTok toks1
            (expr, toks3) <- parseExpr ctx toks2
            return (let var = (Var t name) in ((DeclareAndAssignStmt var expr, putVarInScope ctx var), toks3))
        
        parseWhileLoop :: Either Error ((Stmt, Context), [Token])
        parseWhileLoop = do
            toks1 <- consumeTok WhileTok toks
            toks2 <- consumeTok LParenTok toks1
            (e, toks3) <- parseExpr ctx toks2
            toks4 <- consumeTok RParenTok toks3
            ((block, ctx'), toks5) <- parseBlock ctx toks4
            return $ ((WhileStmt e block, ctx'), toks5)

        parseReturnStmt :: Either Error ((Stmt, Context), [Token])
        parseReturnStmt = do
            toks' <- consumeTok ReturnTok toks
            (expr, toks'') <- parseExpr ctx toks'
            return ((ReturnStmt expr, ctx), toks'')

parseExpr :: Context -> Parser Expr
parseExpr ctx toks = parseMinusExpr ctx toks <|> parseAddExpr ctx toks <|> parseSubtractExpr ctx toks <|> parseMultiplyExpr ctx toks <|> parseAssignExpr ctx toks <|> ((\(a, toks') -> (AtomExpr a, toks')) <$> (parseAtom ctx toks))

parseBinOp :: Context -> Token -> (Atom -> Atom -> Expr) -> Parser Expr
parseBinOp ctx t e toks = do
    (a1, toks') <- parseAtom ctx toks
    toks'' <- consumeTok t toks'
    (a2, toks''') <- parseAtom ctx toks''
    return (e a1 a2, toks''')

parseMinusExpr :: Context -> Parser Expr
parseMinusExpr ctx toks = do
    toks' <- consumeTok MinusTok toks
    (a, toks'') <- parseAtom ctx toks'
    return $ (MinusExpr a, toks'')

parseAddExpr :: Context -> Parser Expr
parseAddExpr ctx = parseBinOp ctx PlusTok AddExpr

parseSubtractExpr :: Context -> Parser Expr
parseSubtractExpr ctx = parseBinOp ctx MinusTok SubtractExpr

parseMultiplyExpr :: Context -> Parser Expr
parseMultiplyExpr ctx = parseBinOp ctx AsteriskTok MultiplyExpr 

parseAssignExpr :: Context -> Parser Expr
parseAssignExpr ctx (NatTok x : EqualsTok : toks) = case varInScope ctx x of
    (Just v) -> do
        (e, toks') <- parseExpr ctx toks
        return (AssignExpr v e, toks')
    Nothing -> Left $ UnexpectedToken x
parseAssignExpr _ _ = Left $ Unexpected


parseAtom :: Context -> Parser Atom
parseAtom ctx toks = parseVarAtom ctx toks <|> parseParenAtom ctx toks <|> parseIntAtom toks <|> parseCharAtom toks

parseVarAtom :: Context -> Parser Atom
parseVarAtom ctx (NatTok name : toks) = case (varInScope ctx name) of 
    Just var -> Right $ (VarAtom var, toks)
    Nothing -> Left Unexpected
parseVarAtom _ _ = Left Unexpected

parseParenAtom :: Context -> Parser Atom
parseParenAtom ctx toks = parseCastAtom <|> parseSubExpr
    where
        parseCastAtom :: Either Error (Atom, [Token])
        --todo implement type casting
        parseCastAtom = Left Unexpected

        parseSubExpr :: Either Error (Atom, [Token])
        parseSubExpr = ((consumeTok LParenTok toks) >>= parseExpr ctx >>= (\(e, toks') -> (,) (ParenAtom e) <$> consumeTok RParenTok toks'))

parseIntAtom :: Parser Atom
parseIntAtom (PrimIntTok x : toks) = Right $ (IntAtom x, toks)
parseIntAtom (t:_) = Left (ExpectedInt t)
parseIntAtom [] = Left NoMoreTokens

parseCharAtom :: Parser Atom
parseCharAtom (CharTok c : toks) = Right $ (CharAtom c, toks)
parseCharAtom (t:_) = Left (ExpectedChar t)
parseCharAtom [] = Left NoMoreTokens

consumeTok :: Token -> [Token] -> Either Error [Token]
consumeTok tok (t:ts)
    | tok == t = pure ts
    | otherwise = Left $ UnexpectedToken (show t)
consumeTok _ _ = Left $ NoMoreTokens


