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

unary :: Parser Char Token
unary = negation <|> factorial

parserForOp2 :: Char -> (Number -> Number -> Number) -> Parser Char Token
parserForOp2 o f (c:s) | c == o = [(Op2 c f, s)]
parserForOp2 _ _ _ = []

binary :: Parser Char Token
binary = foldl1 (<|>) (map (uncurry parserForOp2) binOps)

binOps = [('+',(+)),('-',flip (-)),('*',(*)),('/',(flip div)),('%',flip mod)]

token :: Parser Char Token -> Parser Char [Token]
token pT = \ts -> case pT ts of
    [] -> []
    [(t,ts')] -> [([t],ts')]

seqp :: Parser Char [Token] -> Parser Char [Token] -> Parser Char [Token]
parserA `seqp` parserB = \s -> case parserA s of
    [] -> []
    [(as,s')] -> case parserB s' of
        [] -> []
        [(bs,s'')] -> [(as++bs,s'')]

altp :: Parser Char [Token] -> Parser Char [Token] -> Parser Char [Token]
parserA `altp` parserB =  \s -> case parserA s of
    []        -> parserB s 
    [(as,s')] -> [(as,s')]  

expr :: Parser Char [Token]
expr =  token num  
  `altp` (expr `seqp` expr)
  `altp` (token binary `seqp` expr `seqp` expr)
  `altp` (token unary `seqp` expr)

-- let's try something simpler

data T = N | B | U
    deriving (Eq, Show)

type ParserE = Parser T [T]

n,b,u :: T
(n,b,u) = (N,B,U)


pT :: T -> ParserE
pT token = \ts -> case ts of
    [] -> []
    (t:ts') -> case t == token of
        True -> [([t],ts')]
        False -> [] 
    
a,s :: ParserE -> ParserE -> ParserE

a pA pB = \s -> case pA s of
    []       -> pB s
    r        -> r

s pA pB = \s -> case pA s of
    []       -> []
    [(t,s')] -> case pB s' of
        [(t',s'')] -> [(t++t',s'')]
        [] -> []

pN,pB,pU,pE :: ParserE

pN = pT N
pB = pT B
pU = pT U

pE = pN `a` (pB `s` pE `s` pE) `a` (pU `s` pE)      
