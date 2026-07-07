module Tokeniser where
import Types
import Data.Char
import Text.Read(readMaybe)

tokenstable :: [(Char, Token)]
tokenstable = [
    ('{', LBrace),
    ('}', RBrace),
    (';', SemiColon),
    ('=', Equals),
    ('(', LParen),
    (')', RParen),
    ('-', Minus),
    (',', Comma),
    ('*', Asterisk),
    ('&', Ampersand),
    ('+', Plus),
    ('-', Minus),
    ('>', Greater),
    ('<', Less)]

keywordstable :: [(String, Token)]
keywordstable = [
    ("while", While),
    ("for", For),
    ("void", Void),
    ("struct", Struct),
    ("union", Union),
    ("return", Return),
    ("static", Static),
    ("sizeof", SizeOf)]


flipError :: Either a b -> Either b a
flipError (Left x) = Right x
flipError (Right x) = Left x 

tokeniseLetters :: String -> Either Error Token
tokeniseLetters ls = flipError ((maybeToError (lookup ls keywordstable)) >>= (\_ -> Left (Nat ls)))
    where
        maybeToError :: Maybe Token -> Either Token ()
        maybeToError (Just t) = Left t
        maybeToError Nothing = Right ()

{-
Features included:
    - Keywords
    - Primitives
    - Identifiers
    - Integers
    - Symbols

Missing features:
    - mixed letter/number variable names/types (check spec)
    - hex/binary constants
    - strings
    - chars
    - floats/doubles
-}

tokenise :: String -> Either Error [Token]
tokenise input = reverse <$> tokenise' input []
    where
        tokenise' :: String -> [Token] -> Either Error [Token]
        tokenise' [] toks = Right toks
        tokenise' (c:cs) toks
            | isSpace c = tokenise' cs toks
            | isDigit c = let
                (ds, cs') = span isDigit (c:cs)
                prim = maybe (Left $ IntegerParseError ds) Right (readMaybe ds)
                in prim >>= (\i -> tokenise' cs' (PrimInt i : toks))
            -- check for keywords, primitive types and identifiers
            | isLetter c = let
                (ls, cs') = span isLetter (c:cs)
                in ((:toks) <$> tokeniseLetters ls) >>= tokenise' cs'
            -- a symbol
            | otherwise = maybe (Left (UnexpectedToken (fst(span (not.isSpace) (c:cs))))) (\t -> tokenise' cs (t:toks)) (lookup c tokenstable)
