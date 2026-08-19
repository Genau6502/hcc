module ParserTest where

import TestTypes
import Parser
import Types
import ParseContext

-- Atom tests
parseAtomTests :: TestGroup
parseAtomTests = TestGroup 
    (
        "Parse Atom Tests"
        , [   "Positive integer" -: parseAtom emptyContext [ PrimIntTok 12345 ] --> Right (IntAtom 12345, [])
            , "Alphanumeric chars (1)" -: parseAtom emptyContext [ CharTok 'a' ] --> Right (CharAtom 'a', [])
            , "Alphanumeric chars (2)" -: parseAtom emptyContext [ CharTok '1' ] --> Right (CharAtom '1', [])
            , "Escape chars (1)" -: parseAtom emptyContext [ CharTok '\n' ] --> Right (CharAtom '\n', [])
            , "Escape chars (2)" -: parseAtom emptyContext [ CharTok '\\' ] --> Right (CharAtom '\\', [])
            , "Parenthesis atom" -: parseAtom emptyContext [ LParenTok, PrimIntTok 100, MinusTok, PrimIntTok 1, RParenTok ] --> Right (ParenAtom (SubtractExpr (IntAtom 100) (IntAtom 1)), [])
            , "Var atom" -: parseAtom (putVarInScope emptyContext (Var IntType "x")) [ NatTok "x" ] --> Right (VarAtom (Var IntType "x"), [])
        ]
    )

-- Expr tests
parseExprTests :: TestGroup
parseExprTests = TestGroup 
    (
        "Parse Expr Tests"
        , [ "Sum of two integers" -: parseExpr emptyContext [ PrimIntTok 1, PlusTok, PrimIntTok 2 ] --> Right (AddExpr (IntAtom 1) (IntAtom 2), [])
            , "Negative integer" -: parseExpr emptyContext [ MinusTok, PrimIntTok 12345 ] --> Right (MinusExpr (IntAtom 12345), [])
            , "Subtract two integers" -: parseExpr emptyContext [ PrimIntTok 100, MinusTok, PrimIntTok 1 ] --> Right (SubtractExpr (IntAtom 100) (IntAtom 1), [])
            , "Parenthesis expr" -: parseExpr emptyContext [ PrimIntTok 1, PlusTok, LParenTok, PrimIntTok 100, MinusTok, PrimIntTok 1, RParenTok ] --> Right (AddExpr (IntAtom 1) (ParenAtom (SubtractExpr (IntAtom 100) (IntAtom 1))), [])
        ]
    )

-- Stmt tests
parseStmtTests :: TestGroup
parseStmtTests = TestGroup 
    (
        "Parse Stmt Tests"
        , [ 
            "int x = 2 + 2;" -: parseStmt emptyContext [ NatTok "int", NatTok "x", EqualsTok, PrimIntTok 2, PlusTok, PrimIntTok 2, SemiColonTok ] --> Right ((DeclareAndAssignStmt (Var IntType "x") (AddExpr (IntAtom 2) (IntAtom 2)), [Var IntType "x"]), [])
        ,   "int **x = 0;" -: parseStmt emptyContext [NatTok "int",AsteriskTok,AsteriskTok,NatTok "x",EqualsTok,PrimIntTok 0,SemiColonTok] --> Right ((DeclareAndAssignStmt (Var (PointerType (PointerType IntType)) "x") (AtomExpr (IntAtom 0)), [Var (PointerType (PointerType IntType)) "x"]), [])
        ,   "return 0;" -: parseStmt emptyContext [ReturnTok,PrimIntTok 0,SemiColonTok] --> Right ((ReturnStmt (AtomExpr (IntAtom 0)), []), [])
        ,   "while (1) { return 0; }" -: parseStmt emptyContext [WhileTok,LParenTok,PrimIntTok 1,RParenTok,LBraceTok,ReturnTok,PrimIntTok 0,SemiColonTok,RBraceTok] --> Right ((WhileStmt (AtomExpr (IntAtom 1)) [ReturnStmt (AtomExpr (IntAtom 0))], []), [])
        , "Parse ExprStmt AssignExpr" -: parseStmt [Var IntType "x"] [NatTok "x", EqualsTok, PrimIntTok 5, SemiColonTok] --> Right ((ExprStmt (AssignExpr (Var IntType "x") (AtomExpr (IntAtom 5))), [Var IntType "x"]), [])
        , "Parse ExprStmt Addition" -: parseStmt [] [PrimIntTok 1, PlusTok, PrimIntTok 2, SemiColonTok] --> Right ((ExprStmt (AddExpr (IntAtom 1) (IntAtom 2)), []), [])
        ]
    )

-- Block tests
parseBlockTests :: TestGroup
parseBlockTests = TestGroup
    (
        "Parse block tests",
        [
            "{int x = 1; int y = 2; int z = x + y;}" -: parseBlock emptyContext [LBraceTok,NatTok "int",NatTok "x",EqualsTok,PrimIntTok 1,SemiColonTok,NatTok "int",NatTok "y",EqualsTok,PrimIntTok 2,SemiColonTok,NatTok "int",NatTok "z",EqualsTok,NatTok "x",PlusTok,NatTok "y",SemiColonTok,RBraceTok] --> Right (([DeclareAndAssignStmt (Var IntType "x") (AtomExpr (IntAtom 1)),DeclareAndAssignStmt (Var IntType "y") (AtomExpr (IntAtom 2)),DeclareAndAssignStmt (Var IntType "z") (AddExpr (VarAtom (Var IntType "x")) (VarAtom (Var IntType "y")))],[Var IntType "z",Var IntType "y",Var IntType "x"]),[])
        ]
    )

-- Functions
parseFunctionTests :: TestGroup
parseFunctionTests = TestGroup
    (
        "Parse function tests",
        [
              "(1) Empty function returning int ( int main() {} )" -: parseFunction [NatTok "int", NatTok "main", LParenTok, RParenTok, LBraceTok, RBraceTok] --> Right (Function "main" [] IntType [], [])
            , "(2) Function with multiple arguments ( int add(int a, int b) { return a + b; } )" -: parseFunction [NatTok "int", NatTok "add", LParenTok, NatTok "int", NatTok "a", CommaTok, NatTok "int", NatTok "b", RParenTok, LBraceTok, ReturnTok, NatTok "a", PlusTok, NatTok "b", SemiColonTok, RBraceTok] --> Right (Function "add" [Var IntType "a", Var IntType "b"] IntType [ReturnStmt (AddExpr (VarAtom (Var IntType "a")) (VarAtom (Var IntType "b")))], [])
            , "(3) Function returning a int literal ( int getInt() { return 1; } )" -: parseFunction [NatTok "int", NatTok "getInt", LParenTok, RParenTok, LBraceTok, ReturnTok, PrimIntTok 1, SemiColonTok, RBraceTok] --> Right (Function "getInt" [] IntType [ReturnStmt (AtomExpr (IntAtom 1))], [])
            , "(4) Function with local variable declaration ( int doMaths() { int x = 5; return x; } )" -: parseFunction [NatTok "int", NatTok "doMaths", LParenTok, RParenTok, LBraceTok, NatTok "int", NatTok "x", EqualsTok, PrimIntTok 5, SemiColonTok, ReturnTok, NatTok "x", SemiColonTok, RBraceTok] --> Right (Function "doMaths" [] IntType [DeclareAndAssignStmt (Var IntType "x") (AtomExpr (IntAtom 1)), ReturnStmt (AtomExpr (VarAtom (Var IntType "x")))], [])
        ]
    )

-- Types
parseDeclaratorTests :: TestGroup
parseDeclaratorTests = TestGroup
    (
        "Parse Declarator Tests"
        , [ "int x" -: parseDeclarator emptyContext [ NatTok "int", NatTok "x" ] --> Right (("x", IntType), [])
        , "int *x" -: parseDeclarator emptyContext [ NatTok "int", AsteriskTok, NatTok "x" ] --> Right (("x", PointerType IntType), [])
        , "int x[3]" -: parseDeclarator emptyContext [NatTok "int",NatTok "x",LSqParenTok,PrimIntTok 3,RSqParenTok] --> Right (("x", ArrayType IntType (AtomExpr (IntAtom 3))), [])
        , "int x[3][4]" -: parseDeclarator emptyContext [NatTok "int",NatTok "x",LSqParenTok,PrimIntTok 3,RSqParenTok,LSqParenTok,PrimIntTok 4,RSqParenTok] --> Right (("x", ArrayType (ArrayType IntType (AtomExpr (IntAtom 4))) (AtomExpr (IntAtom 3))), [])
        , "int *x[3][4]" -: parseDeclarator emptyContext [NatTok "int",AsteriskTok,NatTok "x",LSqParenTok,PrimIntTok 3,RSqParenTok,LSqParenTok,PrimIntTok 4,RSqParenTok] --> Right (("x", ArrayType (ArrayType (PointerType IntType) (AtomExpr (IntAtom 4))) (AtomExpr (IntAtom 3))), [])
        , "int (*x)[3]" -: parseDeclarator emptyContext [NatTok "int",LParenTok,AsteriskTok,NatTok "x",RParenTok,LSqParenTok,PrimIntTok 3,RSqParenTok] --> Right (("x", PointerType (ArrayType IntType (AtomExpr (IntAtom 3)))), [])
        , "int (*x[3])[4]" -: parseDeclarator emptyContext [NatTok "int",LParenTok,AsteriskTok,NatTok "x",LSqParenTok,PrimIntTok 3,RSqParenTok,RParenTok,LSqParenTok,PrimIntTok 4,RSqParenTok] --> Right (("x", ArrayType (PointerType (ArrayType IntType (AtomExpr (IntAtom 4)))) (AtomExpr (IntAtom 3))), [])
        , "int **x" -: parseDeclarator emptyContext [NatTok "int",AsteriskTok,AsteriskTok,NatTok "x"] --> Right (("x", PointerType (PointerType IntType)), [])
        , "int x" -: parseDeclarator emptyContext [ NatTok "int", NatTok "x" ] --> Right (("x", IntType), [])
        , "int (*f)(int, int)" -: parseDeclarator emptyContext [NatTok "int",LParenTok,AsteriskTok,NatTok "f",RParenTok,LParenTok,NatTok "int",CommaTok,NatTok "int",RParenTok] --> Right (("f", PointerType (FunctionType [IntType, IntType] IntType)), [])
        , "int *(*get)(int)" -: parseDeclarator emptyContext [NatTok "int",AsteriskTok,LParenTok,AsteriskTok,NatTok "get",RParenTok,LParenTok,NatTok "int",RParenTok] --> Right (("get", PointerType (FunctionType [IntType] (PointerType IntType))), [])
        , "int (*(*p)[4])(int)" -: parseDeclarator emptyContext [NatTok "int",LParenTok,AsteriskTok,LParenTok,AsteriskTok,NatTok "p",RParenTok,LSqParenTok,PrimIntTok 4,RSqParenTok,RParenTok,LParenTok,NatTok "int",RParenTok] --> Right (("p", PointerType (ArrayType (PointerType (FunctionType [IntType] IntType)) (AtomExpr (IntAtom 4)))), [])
        ]
    )

-- Types
parseAbstractDeclaratorTests :: TestGroup
parseAbstractDeclaratorTests = TestGroup
    (
        "Parse Abstract Declarator Tests"
        , [ "int" -: parseAbstractDeclarator emptyContext [ NatTok "int" ] --> Right (IntType, [])
        , "int *" -: parseAbstractDeclarator emptyContext [ NatTok "int", AsteriskTok ] --> Right (PointerType IntType, [])
        , "int [3]" -: parseAbstractDeclarator emptyContext [NatTok "int",LSqParenTok,PrimIntTok 3,RSqParenTok] --> Right (ArrayType IntType (AtomExpr (IntAtom 3)), [])
        , "int [3][4]" -: parseAbstractDeclarator emptyContext [NatTok "int",LSqParenTok,PrimIntTok 3,RSqParenTok,LSqParenTok,PrimIntTok 4,RSqParenTok] --> Right (ArrayType (ArrayType IntType (AtomExpr (IntAtom 4))) (AtomExpr (IntAtom 3)), [])
        , "int *[3][4]" -: parseAbstractDeclarator emptyContext [NatTok "int",AsteriskTok,LSqParenTok,PrimIntTok 3,RSqParenTok,LSqParenTok,PrimIntTok 4,RSqParenTok] --> Right (ArrayType (ArrayType (PointerType IntType) (AtomExpr (IntAtom 4))) (AtomExpr (IntAtom 3)), [])
        , "int (*)[3]" -: parseAbstractDeclarator emptyContext [NatTok "int",LParenTok,AsteriskTok,RParenTok,LSqParenTok,PrimIntTok 3,RSqParenTok] --> Right (PointerType (ArrayType IntType (AtomExpr (IntAtom 3))), [])
        , "int (*[3])[4]" -: parseAbstractDeclarator emptyContext [NatTok "int",LParenTok,AsteriskTok,LSqParenTok,PrimIntTok 3,RSqParenTok,RParenTok,LSqParenTok,PrimIntTok 4,RSqParenTok] --> Right (ArrayType (PointerType (ArrayType IntType (AtomExpr (IntAtom 4)))) (AtomExpr (IntAtom 3)), [])
        , "int **" -: parseAbstractDeclarator emptyContext [NatTok "int",AsteriskTok,AsteriskTok] --> Right (PointerType (PointerType IntType), [])
        , "int" -: parseAbstractDeclarator emptyContext [ NatTok "int" ] --> Right (IntType, [])
        , "int (*)(int, int)" -: parseAbstractDeclarator emptyContext [NatTok "int",LParenTok,AsteriskTok,RParenTok,LParenTok,NatTok "int",CommaTok,NatTok "int",RParenTok] --> Right (PointerType (FunctionType [IntType, IntType] IntType), [])
        , "int *(*)(int)" -: parseAbstractDeclarator emptyContext [NatTok "int",AsteriskTok,LParenTok,AsteriskTok,RParenTok,LParenTok,NatTok "int",RParenTok] --> Right (PointerType (FunctionType [IntType] (PointerType IntType)), [])
        , "int (*(*)[4])(int)" -: parseAbstractDeclarator emptyContext [NatTok "int",LParenTok,AsteriskTok,LParenTok,AsteriskTok,RParenTok,LSqParenTok,PrimIntTok 4,RSqParenTok,RParenTok,LParenTok,NatTok "int",RParenTok] --> Right (PointerType (ArrayType (PointerType (FunctionType [IntType] IntType)) (AtomExpr (IntAtom 4))), [])
        ]
    )
