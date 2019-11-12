import Data.Char
import Data.List

type Number = Integer
type Binary = (Integer -> Integer -> Integer)
type Unary = (Integer -> Integer)
type Symbol = Char

data T = N Number | B Symbol Binary | U Symbol Unary

instance Show T where
    show (N n)   = "N" ++ show n
    show (B c f) = "B" ++ [c]
    show (U c f) = "U" ++ [c]

type Parser a b = [a] -> [(b,[a])]

type ParserE = Parser T [T]

altP, seqP :: Parser a [b] -> Parser a [b] -> Parser a [b]

parserA `altP`  parserB = \s -> case parserA s of
    []       -> parserB s
    r        -> r

parserA `seqP` parserB = \s -> case parserA s of
    []       -> []
    [(t,s')] -> case parserB s' of
        [(t',s'')] -> [(t++t',s'')]
        [] -> []

digit :: Parser Char Number
digit (c:s) | isDigit c = [(toNumber c, s)]
digit _ = []

toNumber = fromIntegral . digitToInt 

accum :: Number -> Parser Char Number
accum n s = case digit s of
    [] -> [(n,s)]
    [(d,s')] -> accum (n * 10 + d) s'

numberToken :: Parser Char T
numberToken (c:s) | isSpace c = numberToken s
numberToken (c:s) | isDigit c = [(N n, s')] where [(n,s')] = accum 0 (c:s)
numberToken _ = []

binaryToken :: Parser Char T
binaryToken (c:s) | isSpace c = binaryToken s
binaryToken ('+':s) = [(B '+' (+), s)]
binaryToken ('*':s) = [(B '*' (*), s)]
binaryToken _ = []

unaryToken :: Parser Char T
unaryToken (c:s) | isSpace c = unaryToken s
unaryToken ('~':s) = [(U '~' negate, s)]
unaryToken ('!':s) = [(U '!' (\n -> product [1..n]), s)]
unaryToken _ = []

parserList :: Parser a b -> Parser a [b]
parserList p = \s -> case p s of
    [] -> []
    [(t,s')] -> [([t],s')]

number = parserList numberToken
binary = parserList binaryToken
unary  = parserList unaryToken

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


