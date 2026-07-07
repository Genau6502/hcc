module Types where

data Error = Unexpected | IntegerParseError String | UnexpectedToken String
    deriving (Eq, Show)

data Token = LBrace | RBrace | SemiColon | Equals | LParen | RParen | PrimInt Int | Void | Struct | Union | Nat String | Comma | Asterisk | Ampersand | While | For | Return | Plus | Minus | Div | Greater | Less | Static | Decimal | SizeOf
    deriving (Eq, Show)