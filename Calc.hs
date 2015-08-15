module Calc
where
import Data.Char (isDigit)
import Data.List (groupBy)
    
type Calc = Either Message Number
type Number = Int
type Message = String

calc :: String -> String
calc = result . foldl (>>=) initial . map parse . tokens
    where
    tokens :: String -> [String]
    tokens = groupBy digits
        where digits c c' = isDigit c && isDigit c'

    initial :: Calc 
    initial = Right 0

    result :: Calc -> String
    result (Right n) = show n
    result (Left m)  = m

    parse :: String -> Number -> Calc
    parse " " = Right . id
    parse "~" = Right . negate
    parse s = case reads s :: [(Number,String)] of
        [(n,_)] -> Right . const n
        []      -> Left  . const (s ++ " ??")
    
