module Types where

data Var = Var Type String | DummyVar Int
    deriving (Eq, Show)

data Type = PointerType Type | StructType String | FunctionType [Type] Type | ArrayType Type Expr | IntType | CharType
    deriving (Eq, Show)

data Atom = IntAtom Int | CharAtom Char | ParenAtom Expr | CastAtom Type Expr | VarAtom Var
    deriving (Eq, Show)

data Expr = AddExpr Atom Atom | MinusExpr Atom | SubtractExpr Atom Atom | MultiplyExpr Atom Atom | AtomExpr Atom | AssignExpr Var Expr | ImplicitExpr {- This is used for array initialisation where size can be inferred later on-}
    deriving (Eq, Show)

data Stmt = DeclareAndAssignStmt Var Expr | AssignStmt Var Expr | ReturnStmt Expr | WhileStmt Expr Block | ExprStmt Expr
    deriving (Eq, Show)

type Block = [Stmt]

data Error = Unexpected | IntegerParseError String | UnexpectedToken String | NoMoreTokens | InvalidChar String | ExpectedChar Token | ExpectedInt Token | InvalidType [Token] | VariableAlreadyDeclared String | MismatchType Type Type
    deriving (Eq, Show)

data Token = LBraceTok | RBraceTok | SemiColonTok | EqualsTok | LParenTok | RParenTok | PrimIntTok Int | CharTok Char | VoidTok | StructTok | UnionTok | NatTok String | CommaTok | AsteriskTok | AmpersandTok | WhileTok | ForTok | ReturnTok | PlusTok | MinusTok | DivTok | GreaterTok | LessTok | StaticTok | DecimalTok | SizeOfTok | LSqParenTok | RSqParenTok
    deriving (Eq, Show)

data Location =
    -- Scratch - caller saved
    R10 | R11
    -- Callee saved
    | R12 | R13 | R14 | R15 | RBX | RBP
    -- Special purpose
    | RSP | RAX
    -- Arguments - caller saved
    | RDI | RSI | RDX | RCX | R8 | R9
    | Stack Int
    | Immediate Int
    deriving Eq

instance Show Location where
    show R8 = "%r8"
    show R9 = "%r9"
    show R10 = "%r10"
    show R11 = "%r11"
    show R12 = "%r12"
    show R13 = "%r13"
    show R14 = "%r14"
    show R15 = "%r15"
    show RDI = "%rdi"
    show RSI = "%rsi"
    show RDX = "%rdx"
    show RCX = "%rcx"
    show RSP = "%rsp"
    show RAX = "%rax"
    show RBX = "%rbx"
    show RBP = "%rbx"
    show (Immediate i) = '$':show i
    show (Stack n) = show n

type RegisterAllocation = [(Var, Location)]
