module Parser(parseAtom, parseExpr, parseStmt, parseDeclarator, parseAbstractDeclarator) where

import Types
type Parser a = [Token] -> Either Error (a, [Token])

(<|>) :: Either a b -> Either a b -> Either a b
Right x <|> _ = Right x
Left _ <|> Right y = Right y
Left x <|> _ = Left x

parseBaseType :: String -> Either Error Type
parseBaseType "int" = pure IntType
parseBaseType _ = Left Unexpected

parseDeclarator :: Parser (String, Type)
parseDeclarator (NatTok x : toks) = do
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
            (e, toks') <- parseExpr toks
            sizeExpr <- parseIntExpr e
            toks'' <- consumeTok RSqParenTok toks'
            (outer, toks''') <- parsePost toks''
            return $ ((\base -> ArrayType (outer base) sizeExpr), toks''')
        parsePost (LParenTok:toks) = do
            (argts, toks') <- parseArgTypes toks
            (outer, toks'') <- parsePost toks'
            return $ ((\base -> FunctionType argts (outer base)), toks'')
        parsePost toks = pure (id, toks)

        parseIntExpr :: Expr -> Either Error Expr
        parseIntExpr e = case (parseTypeOfExpr e) of
            Left err -> Left err
            Right t -> Right e 
parseDeclarator toks = Left $ InvalidType toks

parseAbstractDeclarator :: Parser Type
parseAbstractDeclarator (NatTok x : toks) = do
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
            (e, toks') <- parseExpr toks
            sizeExpr <- parseIntExpr e
            toks'' <- consumeTok RSqParenTok toks'
            (outer, toks''') <- parsePost toks''
            return $ ((\base -> ArrayType (outer base) sizeExpr), toks''')
        parsePost (LParenTok:toks) = do
            (argts, toks') <- parseArgTypes toks
            (outer, toks'') <- parsePost toks'
            return $ ((\base -> FunctionType argts (outer base)), toks'')
        parsePost toks = pure (id, toks)

        parseIntExpr :: Expr -> Either Error Expr
        parseIntExpr e = case (parseTypeOfExpr e) of
            Left err -> Left err
            Right t -> Right e 
parseAbstractDeclarator toks = Left $ InvalidType toks

parseArgTypes :: Parser [Type]
parseArgTypes (RParenTok:toks) = pure ([], toks)
parseArgTypes (NatTok x:toks) = do
    (t, toks') <- parseAbstractDeclarator (NatTok x:toks)
    (ts, toks'') <- parseNextArgType toks'
    return (t:ts, toks'')
parseArgTypes _ = Left $ ExpectedChar RParenTok

parseNextArgType :: Parser [Type]
parseNextArgType (RParenTok:toks) = pure ([], toks)
parseNextArgType (CommaTok:toks) = do
    (ts, toks') <- parseArgTypes toks
    return (ts, toks')
parseNextArgType _ = Left $ Unexpected  

parseTypeOfExpr :: Expr -> Either Error Type
parseTypeOfExpr (AtomExpr (IntAtom _)) = pure IntType
parseTypeOfExpr _ = Left Unexpected

parseStmt :: Parser Stmt
parseStmt toks = parseLineStmt
    where
        parseLineStmt :: Either Error (Stmt, [Token])
        parseLineStmt = do  (stmt, toks') <- parseDeclareAndAssignStmt
                            (toks'') <- consumeTok SemiColonTok toks'
                            return (stmt, toks'')

        parseDeclareAndAssignStmt :: Either Error (Stmt, [Token])
        parseDeclareAndAssignStmt = do
            ((name, t), toks1) <- parseDeclarator toks
            toks2 <- consumeTok EqualsTok toks1
            (expr, toks3) <- parseExpr(toks2)
            return (DeclareAndAssign (Var t name) expr, toks3)

parseExpr :: Parser Expr
parseExpr toks = parseMinusExpr toks <|> parseAddExpr toks <|> parseSubtractExpr toks <|> parseMultiplyExpr toks <|> ((\(a, toks') -> (AtomExpr a, toks')) <$> (parseAtom toks))

parseBinOp :: Token -> (Atom -> Atom -> Expr) -> Parser Expr
parseBinOp t e toks = do
    (a1, toks') <- parseAtom toks
    toks'' <- consumeTok t toks'
    (a2, toks''') <- parseAtom toks''
    return (e a1 a2, toks''')

parseMinusExpr :: Parser Expr
parseMinusExpr toks = do
    toks' <- consumeTok MinusTok toks
    (a, toks'') <- parseAtom toks'
    return $ (MinusExpr a, toks'')

parseAddExpr :: Parser Expr
parseAddExpr = parseBinOp PlusTok AddExpr

parseSubtractExpr :: Parser Expr
parseSubtractExpr = parseBinOp MinusTok SubtractExpr

parseMultiplyExpr :: Parser Expr
parseMultiplyExpr = parseBinOp AsteriskTok MultiplyExpr 

parseAtom :: Parser Atom
parseAtom toks = parseParenAtom toks <|> parseIntAtom toks <|> parseCharAtom toks

parseParenAtom :: Parser Atom
parseParenAtom toks = parseCastAtom <|> parseSubExpr
    where
        parseCastAtom :: Either Error (Atom, [Token])
        --todo implement type casting
        parseCastAtom = Left Unexpected

        parseSubExpr :: Either Error (Atom, [Token])
        parseSubExpr = ((consumeTok LParenTok toks) >>= parseExpr >>= (\(e, toks') -> (,) (ParenAtom e) <$> consumeTok RParenTok toks'))

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