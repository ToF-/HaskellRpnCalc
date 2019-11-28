import Data.Char
import Data.List

type Number = Integer
type Binary = (Integer -> Integer -> Integer)
type Unary = (Integer -> Integer)
type Symbol = Char

data Tk = N Number 
        | B Symbol Binary 
        | U Symbol Unary

data Tree a = Nil | Node a (Tree a) (Tree a)
    deriving (Eq, Show)

instance Show Tk where
    show (N n)   = "N " ++ show n
    show (B c f) = "B " ++ [c]
    show (U c f) = "U " ++ [c]

type Parser a b = [a] -> [(b,[a])]

type ParserE = Parser Tk (Tree Tk)


altP, seqP :: Parser a (Tree b) -> Parser a (Tree b) -> Parser a (Tree b)

(parserA `altP`  parserB) s = case parserA s of
    []       -> parserB s
    r        -> r

(parserA `seqP` parserB) s = case parserA s of
    [] -> []
    rs -> [(grow a b, u) 
          | (a,t) <- rs
          , (b,u) <- parserB t ]

grow :: Tree a -> Tree a -> Tree a
grow Nil t = t
grow (Node a Nil Nil) t = Node a t Nil
grow (Node a l   Nil) t = Node a l t

num :: Parser Char Tk
num s = case reads s of
    [] -> []
    [(n,s)] | n >= 0 -> [(N n, s)]
            | otherwise -> []

binaryOp :: Parser Char Tk
binaryOp (c:s) | isSpace c = binaryOp s
binaryOp ('+':s) = [(B '+' (+), s)]
binaryOp ('*':s) = [(B '*' (*), s)]
binaryOp _ = []

unaryOp :: Parser Char Tk
unaryOp (c:s) | isSpace c = unaryOp s
unaryOp ('-':s) = [(U '-' negate, s)]
unaryOp ('!':s) = [(U '!' (\n -> product [1..n]), s)]
unaryOp _ = []

tree :: Parser a b -> Parser a (Tree b)
tree p = fmap (\(a,s) -> (Node a Nil Nil,s)) . p 

spaces :: Parser Char b -> Parser Char b
spaces p (' ':s) = spaces p s
spaces p s = p s

number = tree (spaces num)
binary = tree (spaces binaryOp)
unary  = tree (spaces unaryOp)

infixl 2 <|>
infixl 3 <&>

(<|>) = altP
(<&>) = seqP
expression = number 
          <|> (unary <&> expression)
          <|> (binary <&> expression <&> expression) 

eval (Node (N n) _ _) = n
eval (Node (U _ f) t _) = f (eval t)
eval (Node (B _ f) t u) = f (eval t) (eval u)

prefix s = case expression s of 
    [] -> error "incorrect prefix expression"
    ((ts,_):_) -> eval ts
