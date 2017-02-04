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
    eval "-"   = binary (flip (-))
    eval "/"   = binary (flip div)
    eval s     = parse s

    param :: Stack -> Calc
    param [] = Left "missing parameter"
    param st = Right st

    change :: (Int -> Int) -> Stack -> Calc
    change f (n:ns) = Right (f n:ns)

    unary :: (Int -> Int) -> Stack -> Calc
    unary f st = param st >>= change f
    
    binary :: (Int -> Int -> Int) -> Stack -> Calc
    binary f (n:ns)   = param ns  >>= change (f n)
    binary f st       = param st
     
    parse :: String -> Stack -> Calc
    parse s ns = case reads s of
        [(n,_)] -> Right (n:ns)
        []      -> Left (s ++ "?")

