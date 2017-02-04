module RpnCalc
where
import Control.Monad

type Calc = Either Message Stack
type Stack = [Int]
type Message = String

calc =  result . foldM (flip eval) [] . words
    where
    result c = either id (show.head) (c >>= unary id)
    
    eval :: String -> Stack -> Calc
    eval "neg" = unary negate
    eval "abs" = unary abs
    eval "+"   = binary (+)
    eval "*"   = binary (*)
    eval s     = parse s

    unary :: (Int -> Int) -> Stack -> Calc
    unary f []     = Left "missing parameter"
    unary f (n:ns) = Right (f n:ns)

    binary :: (Int -> Int -> Int) -> Stack -> Calc
    binary f (n:m:ns) = Right (f n m:ns)
    binary f _        = Left "missing parameter"
     
    parse :: String -> Stack -> Calc
    parse s ns = case reads s of
        [(n,_)] -> Right (n:ns)
        []      -> Left (s ++ "?")

