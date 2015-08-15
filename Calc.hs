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
    parse "~" = Right . (\(n:st) -> negate n:st)
    parse "+" = binary (+) 
    parse "*" = binary (*)
    parse s = case reads s :: [(Number,String)] of
        [(n,_)] -> Right . (n:)
        []      -> Left  . const (s ++ " ??")

    binary :: (Number -> Number -> Number) -> Stack -> Calc
    binary f = Right . (\[n,m] -> [f n m])
    
