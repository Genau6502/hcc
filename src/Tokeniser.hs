module Tokeniser(tokenise) where
import Types
import Data.Char
import Text.Read(readMaybe)

tokenstable :: [(Char, Token)]
tokenstable = [
    ('{', LBraceTok),
    ('}', RBraceTok),
    (';', SemiColonTok),
    ('=', EqualsTok),
    ('(', LParenTok),
    (')', RParenTok),
    ('-', MinusTok),
    (',', CommaTok),
    ('*', AsteriskTok),
    ('&', AmpersandTok),
    ('+', PlusTok),
    ('-', MinusTok),
    ('>', GreaterTok),
    ('<', LessTok),
    ('[', LSqParenTok),
    (']', RSqParenTok)]

keywordstable :: [(String, Token)]
keywordstable = [
    ("while", WhileTok),
    ("for", ForTok),
    ("void", VoidTok),
    ("struct", StructTok),
    ("union", UnionTok),
    ("return", ReturnTok),
    ("static", StaticTok),
    ("sizeof", SizeOfTok)]


flipError :: Either a b -> Either b a
flipError (Left x) = Right x
flipError (Right x) = Left x 

tokeniseLetters :: String -> Either Error Token
tokeniseLetters ls = flipError ((maybeToError (lookup ls keywordstable)) >>= (\_ -> Left (NatTok ls)))
    where
        maybeToError :: Maybe Token -> Either Token ()
        maybeToError (Just t) = Left t
        maybeToError Nothing = Right ()

tokeniseChar :: String -> Either Error (String, Token)
tokeniseChar ('\\':c:'\'':cs) = parseEscape c
    where
        parseEscape :: Char -> Either Error (String, Token)
        parseEscape 'n' = Right (cs, CharTok '\n')
        parseEscape _ = Left $ InvalidChar ('\\':cs)
tokeniseChar (c:'\'':cs) = Right (cs, CharTok c)
tokeniseChar (c:cs) = Left $ InvalidChar (c:cs) 
tokeniseChar [] = Left NoMoreTokens

{-
Features included:
    - Keywords
    - Primitives
    - Identifiers
    - Integers
    - Symbols
    - chars

Missing features:
    - mixed letter/number variable names/types (check spec)
    - hex/binary constants
    - strings
    - floats/doubles
-}

tokenise :: String -> Either Error [Token]
tokenise input = reverse <$> tokenise' input []
    where
        tokenise' :: String -> [Token] -> Either Error [Token]
        tokenise' [] toks = Right toks
        tokenise' ('\'':cs) toks = (((:toks) <$>) <$> tokeniseChar (cs)) >>= (uncurry tokenise')
        tokenise' (c:cs) toks
            | isSpace c = tokenise' cs toks
            | isDigit c = let
                (ds, cs') = span isDigit (c:cs)
                prim = maybe (Left $ IntegerParseError ds) Right (readMaybe ds)
                in prim >>= (\i -> tokenise' cs' (PrimIntTok i : toks))
            -- check for keywords, primitive types and identifiers
            | isLetter c = let
                (ls, cs') = span isLetter (c:cs)
                in ((:toks) <$> tokeniseLetters ls) >>= tokenise' cs'
            -- a symbol
            | otherwise = maybe (Left (UnexpectedToken (fst(span (not.isSpace) (c:cs))))) (\t -> tokenise' cs (t:toks)) (lookup c tokenstable)
