module TokeniserTest where

import TestTypes
import Tokeniser
import Types

tokeniserTests :: TestGroup
tokeniserTests = TestGroup 
    (
        "Tokeniser Tests"
        , [   "Positive Integer" -: tokenise "12345" --> Right [ PrimInt 12345 ]
            , "Negative Integer" -: tokenise "-12345" --> Right [ Minus, PrimInt 12345 ]
            , "Int" -: tokenise "int" --> Right [ Nat "int" ]
            , "Int declaration" -: tokenise "int x = 10;" --> Right [ Nat "int", Nat "x", Equals, PrimInt 10, SemiColon ]
            , "Pointer to int" -: tokenise "int *y = &x;" --> Right [ Nat "int", Asterisk, Nat "y", Equals, Ampersand, Nat "x", SemiColon ]
            , "Pointer to int" -: tokenise "int *y = &x;" --> Right [ Nat "int", Asterisk, Nat "y", Equals, Ampersand, Nat "x", SemiColon ]
            , "Return" -: tokenise "return a + b;" --> Right [ Return, Nat "a", Plus, Nat "b", SemiColon ]
            , "While" -: tokenise "while (a > 0)\n{a--;\n}" --> Right [ While, LParen, Nat "a", Greater, PrimInt 0, RParen, LBrace, Nat "a", Minus, Minus, SemiColon, RBrace ]
            , "Void Pointer" -: tokenise "void *doWork()" --> Right [ Void, Asterisk, Nat "doWork", LParen, RParen ]
            , "Struct" -: tokenise "struct Pair {\nvoid *a;\nunion x *b;\n}" --> Right [ Struct, Nat "Pair", LBrace, Void, Asterisk, Nat "a", SemiColon, Union, Nat "x", Asterisk, Nat "b", SemiColon, RBrace ]
            , "Sizeof" -: tokenise "sizeof(int)" --> Right [ SizeOf, LParen, Nat "int", RParen ]
        ]
    )