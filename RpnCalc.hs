module RpnCalc
where

type Calc = Either Message Int
type Message = String

calc = result . foldl eval initial . words
    where

    result :: Calc -> String
    result (Right x) = show x
    result (Left m)  = m

    initial :: Calc
    initial = Right 0

    eval :: Calc -> String -> Calc
    eval x s = (unary s) x

    unary :: String -> (Calc -> Calc)
    unary "neg" = fmap negate
    unary "abs" = fmap abs
    unary s     = case reads s of
        [(n,_)] -> fmap (const n)
        []      -> const (Left (s ++ "?"))
