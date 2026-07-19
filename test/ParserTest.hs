module ParserTest where

import TestTypes
import Parser
import Types

-- Atom tests
parseAtomTests :: TestGroup
parseAtomTests = TestGroup 
    (
        "Parse Atom Tests"
        , [   "Positive integer" -: parseAtom [ PrimIntTok 12345 ] --> Right (IntAtom 12345, [])
            , "Alphanumeric chars (1)" -: parseAtom [ CharTok 'a' ] --> Right (CharAtom 'a', [])
            , "Alphanumeric chars (2)" -: parseAtom [ CharTok '1' ] --> Right (CharAtom '1', [])
            , "Escape chars (1)" -: parseAtom [ CharTok '\n' ] --> Right (CharAtom '\n', [])
            , "Escape chars (2)" -: parseAtom [ CharTok '\\' ] --> Right (CharAtom '\\', [])
            , "Parenthesis atom" -: parseAtom [ LParenTok, PrimIntTok 100, MinusTok, PrimIntTok 1, RParenTok ] --> Right (ParenAtom (SubtractExpr (IntAtom 100) (IntAtom 1)), [])
        ]
    )

-- Expr tests
parseExprTests :: TestGroup
parseExprTests = TestGroup 
    (
        "Parse Expr Tests"
        , [ "Sum of two integers" -: parseExpr [ PrimIntTok 1, PlusTok, PrimIntTok 2 ] --> Right (AddExpr (IntAtom 1) (IntAtom 2), [])
            , "Negative integer" -: parseExpr [ MinusTok, PrimIntTok 12345 ] --> Right (MinusExpr (IntAtom 12345), [])
            , "Subtract two integers" -: parseExpr [ PrimIntTok 100, MinusTok, PrimIntTok 1 ] --> Right (SubtractExpr (IntAtom 100) (IntAtom 1), [])
            , "Parenthesis expr" -: parseExpr [ PrimIntTok 1, PlusTok, LParenTok, PrimIntTok 100, MinusTok, PrimIntTok 1, RParenTok ] --> Right (AddExpr (IntAtom 1) (ParenAtom (SubtractExpr (IntAtom 100) (IntAtom 1))), [])
        ]
    )

-- Stmt tests
parseStmtTests :: TestGroup
parseStmtTests = TestGroup 
    (
        "Parse Stmt Tests"
        , [ 
            "int x = 2 + 2;" -: parseStmt [ NatTok "int", NatTok "x", EqualsTok, PrimIntTok 2, PlusTok, PrimIntTok 2, SemiColonTok ] --> Right (DeclareAndAssignStmt (Var IntType "x") (AddExpr (IntAtom 2) (IntAtom 2)), [])
        ,   "int **x = 0;" -: parseStmt [NatTok "int",AsteriskTok,AsteriskTok,NatTok "x",EqualsTok,PrimIntTok 0,SemiColonTok] --> Right (DeclareAndAssignStmt (Var (PointerType (PointerType IntType)) "x") (AtomExpr (IntAtom 0)), [])
        ,   "return 0;" -: parseStmt [ReturnTok,PrimIntTok 0,SemiColonTok] --> Right (ReturnStmt (AtomExpr (IntAtom 0)), [])
        ]
    )

-- Types
parseDeclaratorTests :: TestGroup
parseDeclaratorTests = TestGroup
    (
        "Parse Declarator Tests"
        , [ "int x" -: parseDeclarator [ NatTok "int", NatTok "x" ] --> Right (("x", IntType), [])
        , "int *x" -: parseDeclarator [ NatTok "int", AsteriskTok, NatTok "x" ] --> Right (("x", PointerType IntType), [])
        , "int x[3]" -: parseDeclarator [NatTok "int",NatTok "x",LSqParenTok,PrimIntTok 3,RSqParenTok] --> Right (("x", ArrayType IntType (AtomExpr (IntAtom 3))), [])
        , "int x[3][4]" -: parseDeclarator [NatTok "int",NatTok "x",LSqParenTok,PrimIntTok 3,RSqParenTok,LSqParenTok,PrimIntTok 4,RSqParenTok] --> Right (("x", ArrayType (ArrayType IntType (AtomExpr (IntAtom 4))) (AtomExpr (IntAtom 3))), [])
        , "int *x[3][4]" -: parseDeclarator [NatTok "int",AsteriskTok,NatTok "x",LSqParenTok,PrimIntTok 3,RSqParenTok,LSqParenTok,PrimIntTok 4,RSqParenTok] --> Right (("x", ArrayType (ArrayType (PointerType IntType) (AtomExpr (IntAtom 4))) (AtomExpr (IntAtom 3))), [])
        , "int (*x)[3]" -: parseDeclarator [NatTok "int",LParenTok,AsteriskTok,NatTok "x",RParenTok,LSqParenTok,PrimIntTok 3,RSqParenTok] --> Right (("x", PointerType (ArrayType IntType (AtomExpr (IntAtom 3)))), [])
        , "int (*x[3])[4]" -: parseDeclarator [NatTok "int",LParenTok,AsteriskTok,NatTok "x",LSqParenTok,PrimIntTok 3,RSqParenTok,RParenTok,LSqParenTok,PrimIntTok 4,RSqParenTok] --> Right (("x", ArrayType (PointerType (ArrayType IntType (AtomExpr (IntAtom 4)))) (AtomExpr (IntAtom 3))), [])
        , "int **x" -: parseDeclarator [NatTok "int",AsteriskTok,AsteriskTok,NatTok "x"] --> Right (("x", PointerType (PointerType IntType)), [])
        , "int x" -: parseDeclarator [ NatTok "int", NatTok "x" ] --> Right (("x", IntType), [])
        , "int (*f)(int, int)" -: parseDeclarator [NatTok "int",LParenTok,AsteriskTok,NatTok "f",RParenTok,LParenTok,NatTok "int",CommaTok,NatTok "int",RParenTok] --> Right (("f", PointerType (FunctionType [IntType, IntType] IntType)), [])
        , "int *(*get)(int)" -: parseDeclarator [NatTok "int",AsteriskTok,LParenTok,AsteriskTok,NatTok "get",RParenTok,LParenTok,NatTok "int",RParenTok] --> Right (("get", PointerType (FunctionType [IntType] (PointerType IntType))), [])
        , "int (*(*p)[4])(int)" -: parseDeclarator [NatTok "int",LParenTok,AsteriskTok,LParenTok,AsteriskTok,NatTok "p",RParenTok,LSqParenTok,PrimIntTok 4,RSqParenTok,RParenTok,LParenTok,NatTok "int",RParenTok] --> Right (("p", PointerType (ArrayType (PointerType (FunctionType [IntType] IntType)) (AtomExpr (IntAtom 4)))), [])
        ]
    )

-- Types
parseAbstractDeclaratorTests :: TestGroup
parseAbstractDeclaratorTests = TestGroup
    (
        "Parse Abstract Declarator Tests"
        , [ "int" -: parseAbstractDeclarator [ NatTok "int" ] --> Right (IntType, [])
        , "int *" -: parseAbstractDeclarator [ NatTok "int", AsteriskTok ] --> Right (PointerType IntType, [])
        , "int [3]" -: parseAbstractDeclarator [NatTok "int",LSqParenTok,PrimIntTok 3,RSqParenTok] --> Right (ArrayType IntType (AtomExpr (IntAtom 3)), [])
        , "int [3][4]" -: parseAbstractDeclarator [NatTok "int",LSqParenTok,PrimIntTok 3,RSqParenTok,LSqParenTok,PrimIntTok 4,RSqParenTok] --> Right (ArrayType (ArrayType IntType (AtomExpr (IntAtom 4))) (AtomExpr (IntAtom 3)), [])
        , "int *[3][4]" -: parseAbstractDeclarator [NatTok "int",AsteriskTok,LSqParenTok,PrimIntTok 3,RSqParenTok,LSqParenTok,PrimIntTok 4,RSqParenTok] --> Right (ArrayType (ArrayType (PointerType IntType) (AtomExpr (IntAtom 4))) (AtomExpr (IntAtom 3)), [])
        , "int (*)[3]" -: parseAbstractDeclarator [NatTok "int",LParenTok,AsteriskTok,RParenTok,LSqParenTok,PrimIntTok 3,RSqParenTok] --> Right (PointerType (ArrayType IntType (AtomExpr (IntAtom 3))), [])
        , "int (*[3])[4]" -: parseAbstractDeclarator [NatTok "int",LParenTok,AsteriskTok,LSqParenTok,PrimIntTok 3,RSqParenTok,RParenTok,LSqParenTok,PrimIntTok 4,RSqParenTok] --> Right (ArrayType (PointerType (ArrayType IntType (AtomExpr (IntAtom 4)))) (AtomExpr (IntAtom 3)), [])
        , "int **" -: parseAbstractDeclarator [NatTok "int",AsteriskTok,AsteriskTok] --> Right (PointerType (PointerType IntType), [])
        , "int" -: parseAbstractDeclarator [ NatTok "int" ] --> Right (IntType, [])
        , "int (*)(int, int)" -: parseAbstractDeclarator [NatTok "int",LParenTok,AsteriskTok,RParenTok,LParenTok,NatTok "int",CommaTok,NatTok "int",RParenTok] --> Right (PointerType (FunctionType [IntType, IntType] IntType), [])
        , "int *(*)(int)" -: parseAbstractDeclarator [NatTok "int",AsteriskTok,LParenTok,AsteriskTok,RParenTok,LParenTok,NatTok "int",RParenTok] --> Right (PointerType (FunctionType [IntType] (PointerType IntType)), [])
        , "int (*(*)[4])(int)" -: parseAbstractDeclarator [NatTok "int",LParenTok,AsteriskTok,LParenTok,AsteriskTok,RParenTok,LSqParenTok,PrimIntTok 4,RSqParenTok,RParenTok,LParenTok,NatTok "int",RParenTok] --> Right (PointerType (ArrayType (PointerType (FunctionType [IntType] IntType)) (AtomExpr (IntAtom 4))), [])
        ]
    )
