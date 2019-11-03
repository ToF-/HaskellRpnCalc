import Data.Char
import Data.List


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
  `altp` ((token binary `seqp` expr) `seqp` expr)
  `altp` (token unary `seqp` expr)

-- let's try something simpler

type Number = Integer
type Binary = (Integer -> Integer -> Integer)
type Unary = (Integer -> Integer)
type Symbol = Char

data T = N Number | B Symbol Binary | U Symbol Unary

instance Show T where
    show (N n) = "N" ++ show n
    show (B c f) = "B" ++ [c]
    show (U c f) = "U" ++ [c]

type ParserE = Parser T [T]


a,s :: Parser a [b] -> Parser a [b] -> Parser a [b]

a pA pB = \s -> case pA s of
    []       -> pB s
    r        -> r

s pA pB = \s -> case pA s of
    []       -> []
    [(t,s')] -> case pB s' of
        [(t',s'')] -> [(t++t',s'')]
        [] -> []

n1,n2,pl,fa :: T
n1 = N 42
n2 = N 17
pl = B '+' (+)
fa = U '!' (\n -> product [1..n])

pN,pB,pU,pE :: ParserE

pN (N n:ts)  = [([N n],ts)]
pN _ = []

pB (B '+' f:ts) = [([B '+' f],ts)]
pB _ = [] 

pU (U '!' f:ts) = [([U '!' f],ts)]
pU _ = [] 

pE = pN `a` (pB `s` pE `s` pE) `a` (pU `s` pE)      

test = pE [B '+' (+),N 42,U '!' (\n -> product [1..n]),N 17]

pD :: Parser Char Number
pD (c:s) | isDigit c = [(toNumber c, s)]
pD _ = []

toNumber = fromIntegral . digitToInt 

pA :: Number -> Parser Char Number
pA n s = case pD s of
    [] -> [(n,s)]
    [(d,s')] -> pA (n * 10 + d) s'

pNT :: Parser Char T
pNT (c:s) | isSpace c = pNT s
pNT (c:s) | isDigit c = [(N n, s')] where [(n,s')] = pA 0 (c:s)
pNT _ = []

pBT :: Parser Char T
pBT (c:s) | isSpace c = pBT s
pBT ('+':s) = [(B '+' (+), s)]
pBT ('*':s) = [(B '*' (*), s)]
pBT _ = []

pUT :: Parser Char T
pUT (c:s) | isSpace c = pUT s
pUT ('~':s) = [(U '~' negate, s)]
pUT ('!':s) = [(U '!' (\n -> product [1..n]), s)]
pUT _ = []

pL :: Parser a b -> Parser a [b]
pL p = \s -> case p s of
    [] -> []
    [(t,s')] -> [([t],s')]

pLE = (pL pNT) `a` (pL pBT `s` pLE `s` pLE) `a` (pL pUT `s` pLE)
