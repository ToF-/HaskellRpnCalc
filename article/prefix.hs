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

num :: Parser Char Token
num (c:s) | isSpace c = num s
num (c:s) | isDigit c = [(Num n,s')] where [(n,s')] = accum 0 (c:s)
num _ = []

parserForOp1 :: Char -> (Number -> Number) -> Parser Char Token
parserForOp1 o f (c:s) | c == o = [(Op1 c f, s)]
parserForOp1 _ _ _ = []

negation = parserForOp1 '~' negate
factorial = parserForOp1 '!' (\n -> product [1..n])

infix 2 <|>
 
(<|>) :: Parser a b -> Parser a b -> Parser a b
parserA <|> parserB = \s -> case parserA s of
    [] -> parserB s
    r  -> r

unary = negation <|> factorial

parserForOp2 :: Char -> (Number -> Number -> Number) -> Parser Char Token
parserForOp2 o f (c:s) | c == o = [(Op2 c f, s)]
parserForOp2 _ _ _ = []

binary = foldl1 (<|>) (map (uncurry parserForOp2) binOps)

binOps = [('+',(+)),('-',flip (-)),('*',(*)),('/',(flip div)),('%',flip mod)]

token :: Parser a b -> Parser a [b]
token parser = \s -> case parser s of
    [(a,s')] -> [([a],s')]
    [] -> []

seqp :: Parser a [b] -> Parser a [b] -> Parser a [b]
parserA `seqp` parserB = \s -> case parserA s of
    [] -> []
    [(as,s')] -> case parserB s' of
        [] -> []
        [(bs,s'')] -> [(as++bs,s'')]

altp :: Parser a [b] -> Parser a [b] -> Parser a [b]
parserA `altp` parserB =  \s -> case parserA s of
    [(as,s')] -> [(as,s')]  
    [] -> case parserB s of
        [(bs,s')] -> [(bs,s')]
        [] -> []

expr :: Parser Char [Token]
expr =  token num 
  `altp` (token binary `seqp` expr `seqp` expr)
  `altp` (token unary `seqp` expr)
