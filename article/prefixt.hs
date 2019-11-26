import Data.Char
import Data.List

type Number = Integer
type Binary = (Integer -> Integer -> Integer)
type Unary = (Integer -> Integer)
type Symbol = Char

data Tk = N Number | B Symbol Binary | U Symbol Unary

data Tree a = Nil | Node a (Tree a) (Tree a)
    deriving (Eq, Show)

instance Show Tk where
    show (N n)   = "N" ++ show n
    show (B c f) = "B" ++ [c]
    show (U c f) = "U" ++ [c]

type Parser a b = [a] -> [(b,[a])]

type ParserE = Parser Tk (Tree Tk)

altP, seqP :: Parser a (Tree b) -> Parser a (Tree b) -> Parser a (Tree b)

parserA `altP`  parserB = \s -> case parserA s of
    []       -> parserB s
    r        -> r

parserA `seqP` parserB = \s -> case parserA s of
    []       -> []
    [(t,s')] -> case parserB s' of
                  [(t',s'')] -> [(grow t t',s'')]
                  [] -> []

grow :: Tree a -> Tree a -> Tree a
grow Nil t = t
grow (Node a Nil Nil) t = Node a t Nil
grow (Node a l   Nil) t = Node a l t

numberToken :: Parser Char Tk
numberToken s = case reads s of
    [] -> []
    [(n,s)] | n >= 0 -> [(N n, s)]
            | otherwise -> []

binaryToken :: Parser Char Tk
binaryToken (c:s) | isSpace c = binaryToken s
binaryToken ('+':s) = [(B '+' (+), s)]
binaryToken ('*':s) = [(B '*' (*), s)]
binaryToken _ = []

unaryToken :: Parser Char Tk
unaryToken (c:s) | isSpace c = unaryToken s
unaryToken ('~':s) = [(U '~' negate, s)]
unaryToken ('!':s) = [(U '!' (\n -> product [1..n]), s)]
unaryToken _ = []

parserList :: Parser a b -> Parser a [b]
parserList p = \s -> case p s of
    [] -> []
    [(t,s')] -> [([t],s')]

parserTree :: Parser a b -> Parser a (Tree b)
parserTree p = \s -> case p s of
                       [] -> []
                       [(a,s)] -> [(Node a Nil Nil,s)]

number = parserTree numberToken
binary = parserTree binaryToken
unary  = parserTree unaryToken

infixl 2 <|>
infixl 3 <.>

(<|>) = altP
(<.>) = seqP
expression = number 
          <|> binary <.> expression <.> expression 
          <|> unary <.> expression

newtype P a = P { parse :: String -> [(a, String)] }

instance Functor P where
    fmap f m = P $ \s -> [ (f a, s') 
                     | (a,s') <- parse m s]

instance Applicative P where
    pure = return
    (<*>) = undefined

    
instance Monad P where
    return t = P $ \s -> [(t,s)]
    m >>= k = P $ \s -> [ (b,s'') 
                      | (a,s') <- parse m s, 
                        (b,s'') <- parse (k a) s'] 


