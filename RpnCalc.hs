module RpnCalc
where

type Stack = [Integer]
data Token = Const Integer
           | Unary (Integer -> Integer)
           | Binary (Integer -> Integer -> Integer)

eval :: Stack -> Token -> Stack
eval st       (Const n)  = n     : st
eval (n:ns)   (Unary f)  = f n   : ns
eval (n:m:ns) (Binary f) = f n m : ns

parse s    = case reads s :: [(Integer,String)] of
    [(n,r)] -> [(Const n,r)]
    []      -> parseFunction s
    where
    parseFunction ('!':s) = [(Unary (\n -> product [1..n]),s)]
    parseFunction ('+':s) = [(Binary (+),s)]

parseRPN :: String -> [Token]
parseRPN = parseIt []
    where
    parseIt :: [Token] -> String -> [Token]
    parseIt ts "" = ts
    parseIt ts s = let
        [(t,s')] = parse s
        in parseIt (ts ++ [t]) s'
