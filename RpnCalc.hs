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
    unary "neg" = \c -> case c of
        (Right [n]) -> Right [negate n]
        (Right [] ) -> Left "missing parameter"
        Left m      -> Left m
    unary "abs" = \c -> case c of
        (Right [n]) -> Right [abs n]
        (Right [] ) -> Left "missing parameter"
        Left m      -> Left m
    unary s     = case reads s of
        [(n,_)] -> fmap (n:)
        []      -> const (Left (s ++ "?"))

    checkParam :: Calc -> Calc
    checkParam (Right []) = Left "missing parameter"
    checkParam c          = c
