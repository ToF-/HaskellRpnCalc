module RpnCalc
where

type Calc = Either Message [Int]
type Message = String

calc = result . foldl eval initial . words
    where

    result :: Calc -> String
    result (Right x) = show (head x)
    result (Left m)  = m

    initial :: Calc
    initial = Right []

    eval :: Calc -> String -> Calc
    eval x s = (unary s) x

    unary :: String -> Calc -> Calc
    unary "neg" c = c >>= pop >>= top negate
    unary "abs" c = c >>= pop >>= top abs
    unary s     c = c >>= parse s

    parse :: String -> [Int] -> Calc
    parse s ns = case reads s of
        [(n,_)] -> Right (n:ns)
        []      -> Left (s ++ "?")

    top :: (Int -> Int) -> (Int,[Int]) -> Calc
    top f (n,ns) = Right (f n:ns)

    pop :: [Int] -> Either Message (Int,[Int])
    pop (n:ns) = Right (n,ns)
    pop []     = Left "missing parameter"
