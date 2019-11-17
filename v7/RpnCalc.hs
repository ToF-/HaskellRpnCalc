module RpnCalc
where
import Data.Char

type Stack = [Integer]
data Token = Const Integer
           | Unary (Integer -> Integer)
           | Binary (Integer -> Integer -> Integer)

eval :: Stack -> Token -> Stack
eval st       (Const n)  = n     : st
eval (n:ns)   (Unary f)  = f n   : ns
eval (n:m:ns) (Binary f) = f n m : ns

evalRPN :: [Token] -> Integer
evalRPN = head . foldl eval []

parse s    = case digits of
    [] -> parseFunction s
    _  -> [(Const (read digits),drop (length digits) s)]
    where
    digits = takeWhile isDigit s
    parseFunction (' ':s) = [(Unary id,s)]
    parseFunction ('!':s) = [(Unary (\n -> product [1..n]),s)]
    parseFunction ('~':s) = [(Unary negate,s)]
    parseFunction ('+':s) = [(Binary (+),s)]
    parseFunction ('*':s) = [(Binary (*),s)]
    parseFunction ('-':s) = [(Binary (flip ( - )),s)]
    parseFunction ('/':s) = [(Binary (flip div),s)]
    parseFunction ('%':s) = [(Binary (flip mod),s)]
    parseFunction ('^':s) = [(Binary (flip (^)),s)]

parseRPN :: String -> [Token]
parseRPN = parseIt []
    where
    parseIt :: [Token] -> String -> [Token]
    parseIt ts "" = ts
    parseIt ts s = let
        [(t,s')] = parse s
        in parseIt (ts ++ [t]) s'
