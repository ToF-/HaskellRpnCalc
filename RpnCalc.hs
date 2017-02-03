module RpnCalc
where

calc = show . foldl eval 0 . words
    where
    eval x s = unary s x
    unary "neg" = negate
    unary "abs" = abs
    unary s     = const (read s)
