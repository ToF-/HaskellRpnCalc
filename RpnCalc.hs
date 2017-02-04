module RpnCalc
where
import Control.Monad

type Calc = Either Message Stack
type Stack = [Int]
type Message = String

calc =  result . foldM eval [] . words
    where
    result = either id (show.head)
    
    eval :: Stack -> String -> Calc
    eval st "neg" = pop st >>= top negate
    eval st "abs" = pop st >>= top abs
    eval st s     = parse s st
     
    parse :: String -> Stack -> Calc
    parse s ns = case reads s of
        [(n,_)] -> Right (n:ns)
        []      -> Left (s ++ "?")

    top :: (Int -> Int) -> (Int,Stack) -> Calc
    top f (n,ns) = Right (f n:ns)

    pop :: Stack -> Either Message (Int,Stack)
    pop (n:ns) = Right (n,ns)
    pop []     = Left "missing parameter"
