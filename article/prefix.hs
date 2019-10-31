import Data.Char
import Data.List

type Number = Integer

data Token = Num Number
           | Op1 Char (Number -> Number)
           | Op2 Char (Number -> Number -> Number)

instance Show Token
    where
    show (Num n) = "Num " ++ show n
    show (Op1 c f) = "Op1 " ++ [c] 
    show (Op2 c f) = "Op2 " ++ [c] 

type Parser a b = [a] -> [(b,[a])]

digit :: Parser Char Number
digit (c:s) | isDigit c = [(fromIntegral (digitToInt c), s)]
digit _ = []

accum :: Number -> Parser Char Number
accum n s = case digit s of
    [] -> [(n,s)]
    [(d,s')] -> accum (n * 10 + d) s'

number :: Parser Char Token
number (c:s) | isSpace c = number s
number (c:s) | isDigit c = [(Num n,s')] where [(n,s')] = accum 0 (c:s)
number _ = []

operator1 :: Char -> (Number -> Number) -> Parser Char Token
operator1 o f (c:s) | c == o = [(Op1 c f, s)]
operator1 _ _ _ = []

negation = operator1 '~' negate
factorial = operator1 '!' (\n -> product [1..n])

infix 2 <|>
 
(<|>) :: Parser a b -> Parser a b -> Parser a b
parserA <|> parserB = \s -> case parserA s of
    [] -> parserB s
    r  -> r

unary = negation <|> factorial

operator2 :: Char -> (Number -> Number -> Number) -> Parser Char Token
operator2 o f (c:s) | c == o = [(Op2 c f, s)]
operator2 _ _ _ = []

binary = foldl1 (<|>) (map (uncurry operator2) binOps)

binOps = [('+',(+)),('-',flip (-)),('*',(*)),('/',(flip div)),('%',flip mod)]

infix 3 <&>

(<&>) :: Parser a b -> Parser a b -> Parser a b
parserA <&> parserB = \s -> case parserA s of
    [] -> []
    [(a,s')] -> parserB s'


num :: [Token] -> Parser Char [Token]
num ts s = case number s of
    [] -> [(ts,s)]
    [(t,s')] -> [(ts ++ [t], s')]

unaryOp :: [Token] -> Parser Char [Token] 
unaryOp ts s = case unary s of
    [(t,s')] -> [(ts ++ [t], s')]
    [] -> [(ts,s)] 
