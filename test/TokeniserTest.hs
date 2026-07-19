module TokeniserTest where

import TestTypes
import Tokeniser
import Types

tokeniserTests :: TestGroup
tokeniserTests = TestGroup 
    (
        "Tokeniser Tests"
        , [   "Positive Integer" -: tokenise "12345" --> Right [ PrimIntTok 12345 ]
            , "Negative Integer" -: tokenise "-12345" --> Right [ MinusTok, PrimIntTok 12345 ]
            , "Int" -: tokenise "int" --> Right [ NatTok "int" ]
            , "Int declaration" -: tokenise "int x = 10;" --> Right [ NatTok "int", NatTok "x", EqualsTok, PrimIntTok 10, SemiColonTok ]
            , "Pointer to int" -: tokenise "int *y = &x;" --> Right [ NatTok "int", AsteriskTok, NatTok "y", EqualsTok, AmpersandTok, NatTok "x", SemiColonTok ]
            , "Pointer to int" -: tokenise "int *y = &x;" --> Right [ NatTok "int", AsteriskTok, NatTok "y", EqualsTok, AmpersandTok, NatTok "x", SemiColonTok ]
            , "Return" -: tokenise "return a + b;" --> Right [ ReturnTok, NatTok "a", PlusTok, NatTok "b", SemiColonTok ]
            , "While" -: tokenise "while (a > 0)\n{a--;\n}" --> Right [ WhileTok, LParenTok, NatTok "a", GreaterTok, PrimIntTok 0, RParenTok, LBraceTok, NatTok "a", MinusTok, MinusTok, SemiColonTok, RBraceTok ]
            , "Void Pointer" -: tokenise "void *doWork()" --> Right [ VoidTok, AsteriskTok, NatTok "doWork", LParenTok, RParenTok ]
            , "Struct" -: tokenise "struct Pair {\nvoid *a;\nunion x *b;\n}" --> Right [ StructTok, NatTok "Pair", LBraceTok, VoidTok, AsteriskTok, NatTok "a", SemiColonTok, UnionTok, NatTok "x", AsteriskTok, NatTok "b", SemiColonTok, RBraceTok ]
            , "Sizeof" -: tokenise "sizeof(int)" --> Right [ SizeOfTok, LParenTok, NatTok "int", RParenTok ]
            , "Characters" -: tokenise "\'a\';\'\\n\';" --> Right [CharTok 'a', SemiColonTok, CharTok '\n', SemiColonTok]
        ]
    )