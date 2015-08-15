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
    parse "-" = binary (flip (-)) 
    parse "*" = binary (*)
    parse "/" = binary (flip div)
    parse "%" = binary (flip mod)
    parse s = case reads s :: [(Number,String)] of
        [(n,_)] -> Right . (n:)
        []      -> Left  . const (s ++ " ??")

    binary :: (Number -> Number -> Number) -> Stack -> Calc
    binary f st = pull st >>= \(n,st') -> unary (f n) st'

    unary :: (Number -> Number) -> Stack -> Calc
    unary f st = pull st >>= \(n,st') -> push (f n) st'

    push :: Number -> Stack -> Calc
    push n st = Right (n:st)
    
    pull :: Stack -> Either Message (Number,Stack)
    pull [] = Left "not enough parameters"
    pull (n:ns) = Right (n,ns)
    
