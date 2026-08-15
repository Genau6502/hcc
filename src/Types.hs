module Types where

data Var = Var Type String | DummyVar Int
    deriving (Eq, Show)

data Type = PointerType Type | StructType String | FunctionType [Type] Type | ArrayType Type Expr | IntType | CharType
    deriving (Eq, Show)

data Atom = IntAtom Int | CharAtom Char | ParenAtom Expr | CastAtom Type Expr | VarAtom Var
    deriving (Eq, Show)

data Expr = AddExpr Atom Atom | MinusExpr Atom | SubtractExpr Atom Atom | MultiplyExpr Atom Atom | AtomExpr Atom | ImplicitExpr {- This is used for array initialisation where size can be inferred later on-}
    deriving (Eq, Show)

data Stmt = DeclareAndAssignStmt Var Expr | AssignStmt Var Expr | ReturnStmt Expr | WhileStmt Expr Block
    deriving (Eq, Show)

type Block = [Stmt]

data Error = Unexpected | IntegerParseError String | UnexpectedToken String | NoMoreTokens | InvalidChar String | ExpectedChar Token | ExpectedInt Token | InvalidType [Token] | VariableAlreadyDeclared String | MismatchType Type Type
    deriving (Eq, Show)

data Token = LBraceTok | RBraceTok | SemiColonTok | EqualsTok | LParenTok | RParenTok | PrimIntTok Int | CharTok Char | VoidTok | StructTok | UnionTok | NatTok String | CommaTok | AsteriskTok | AmpersandTok | WhileTok | ForTok | ReturnTok | PlusTok | MinusTok | DivTok | GreaterTok | LessTok | StaticTok | DecimalTok | SizeOfTok | LSqParenTok | RSqParenTok
    deriving (Eq, Show)

data Location = R12 | R13 | R14 | R15 | DummyReg Int | Stack Int | Immediate Int
    deriving Eq

instance Show Location where
    show R12 = "R12"
    show R13 = "R13"
    show R14 = "R14"
    show R15 = "R15"
    show (Stack n) = "Stack " ++ show n
    show (DummyReg n) = "Dummy_" ++ show n
    show (Immediate i) = '$':(show i)

type RegisterAllocation = [(Var, Location)]
