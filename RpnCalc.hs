module RpnCalc
where

calc = show . eval . words
    where
    eval [s,"neg"] = negate (read s) 
    eval [s]       = read s

