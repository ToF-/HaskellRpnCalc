module Calc
where
import Data.Char (isDigit)
import Data.List (groupBy)
    
type Calc = Either Message Stack
type Stack = [Number]
type Number = Int
type Message = String

calc :: String -> String
calc = result . foldl (>>=) initial . map parse . tokens
    where
    tokens :: String -> [String]
    tokens = groupBy digits
        where digits c c' = isDigit c && isDigit c'

    initial :: Calc 
    initial = Right []

    result :: Calc -> String
    result (Right st) = show (head st)
    result (Left m)  = m

    parse :: String -> Stack -> Calc
    parse " " = Right . id
    parse "~" = unary negate
    parse "+" = binary (+) 
    parse "*" = binary (*)
    parse s = case reads s :: [(Number,String)] of
        [(n,_)] -> Right . (n:)
        []      -> Left  . const (s ++ " ??")

    binary :: (Number -> Number -> Number) -> Stack -> Calc
    binary f st = unary (f n) st'
        where
        (n,st') = pull st    
        pull :: Stack -> (Number,Stack)
        pull (n:ns) = (n,ns)
    
    unary :: (Number -> Number) -> Stack -> Calc
    unary f = check $ Right . (\(n:st) -> f n:st)
    
    check :: (Stack -> Calc) -> Stack -> Calc
    check f [] = Left "not enough parameters"
    check f st = f st
