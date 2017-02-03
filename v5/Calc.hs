module Calc
where
import Data.Char (isDigit)
import Data.List (groupBy)
    
type Calc = Either Message Stack
type Stack = [Number]
type Number = Int
type Message = String
type Token = String
type Operation = Stack -> Calc

calc :: String -> String
calc = result . foldl execute initial . map parse . tokens
    where
    tokens :: String -> [Token]
    tokens = groupBy digits
        where digits c c' = isDigit c && isDigit c'

    initial :: Calc 
    initial = Right []

    execute :: Calc -> Operation -> Calc
    execute = (>>=)

    result :: Calc -> String
    result (Right st) = show (head st)
    result (Left m)  = m

    parse :: Token -> Operation
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

    binary :: (Number -> Number -> Number) -> Operation
    binary f st = pull st >>= \(n,st') -> unary (f n) st'

    unary :: (Number -> Number) -> Operation
    unary f st = pull st >>= \(n,st') -> push (f n) st'

    push :: Number -> Operation
    push n st = Right (n:st)
    
    pull :: Stack -> Either Message (Number,Stack)
    pull [] = Left "not enough parameters"
    pull (n:ns) = Right (n,ns)
    
