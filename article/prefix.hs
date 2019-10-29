import Data.Char
import Data.List

type Parser = String -> (Integer,String)

trim :: String -> String
trim = dropWhile (==' ')

number :: String -> (Integer,String)
number s = let (n,s') = break (not . isDigit) (trim s) in (read n,s')

factorial n = product [1..n]

unaryOperation :: (Integer -> Integer) -> Parser
unaryOperation f s = let (n,s') = expression s in (f n, s')

binaryOperation :: (Integer -> Integer -> Integer) -> Parser
binaryOperation f s = let (n,s') = expression s in unaryOperation (f n) (trim s')

unaryOperators = [('!',factorial),('~',negate)]
binaryOperators = [('+',(+)),('-',(flip (-))),('*',(*)),('/',flip div),('%', flip mod)]

expression :: Parser 
expression (' ':cs) = expression cs
expression (c:cs) = case lookup c unaryOperators of
    Just f -> unaryOperation f cs
    Nothing -> case lookup c binaryOperators of
        Just f -> binaryOperation f cs
        Nothing ->  number (c:cs)

same f x y = f x == f y

reverseIfNotNumber (c:cs) | isDigit c = c:cs
reverseIfNotNumber s = reverse s

prefix = concat . reverse . map reverseIfNotNumber . groupBy (same isDigit) 
