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

    unary :: String -> (Calc -> Calc)
    unary "neg" = fmap (top negate) . checkParam
    unary "abs" = fmap (top abs) . checkParam
    unary s     = case reads s of
        [(n,_)] -> fmap (n:)
        []      -> const (Left (s ++ "?"))

    checkParam :: Calc -> Calc
    checkParam (Right []) = Left "missing parameter"
    checkParam c          = c

    top :: (Int -> Int) -> [Int] -> [Int]
    top f (n:ns)= (f n:ns)
