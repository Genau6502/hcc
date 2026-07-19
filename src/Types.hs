module Types where

data Var = Var Type String
    deriving (Eq, Show)

data Type = PointerType Type | StructType String | FunctionType [Type] Type | ArrayType Type Expr | IntType
    deriving (Eq, Show)

data Atom = IntAtom Int | CharAtom Char | ParenAtom Expr | ValAtom String | TypeAtom Type | Cast Type
    deriving (Eq, Show)

data Expr = AddExpr Atom Atom | MinusExpr Atom | SubtractExpr Atom Atom | MultiplyExpr Atom Atom | AtomExpr Atom | ImplicitExpr {- This is used for array initialisation where size can be inferred later on-}
    deriving (Eq, Show)

data Stmt = DeclareAndAssign Var Expr
    deriving (Eq, Show)

data Error = Unexpected | IntegerParseError String | UnexpectedToken String | NoMoreTokens | InvalidChar String | ExpectedChar Token | ExpectedInt Token | InvalidType [Token]
    deriving (Eq, Show)

data Token = LBraceTok | RBraceTok | SemiColonTok | EqualsTok | LParenTok | RParenTok | PrimIntTok Int | CharTok Char | VoidTok | StructTok | UnionTok | NatTok String | CommaTok | AsteriskTok | AmpersandTok | WhileTok | ForTok | ReturnTok | PlusTok | MinusTok | DivTok | GreaterTok | LessTok | StaticTok | DecimalTok | SizeOfTok | LSqParenTok | RSqParenTok
    deriving (Eq, Show)