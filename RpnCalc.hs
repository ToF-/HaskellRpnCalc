module RpnCalc
where

calc = show . foldl eval 0 . words
    where
    eval x "neg" = negate x
    eval x "abs" = abs x
    eval x s     = read s
