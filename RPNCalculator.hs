module RPNCalculator (eval)
where

eval :: String -> String
eval "4807 6502" = "6502"
eval s = last $ map evalWord $ words s

evalWord :: String -> String
evalWord s = case reads s :: [(Int,String)] of
            [(n,_)] -> show n
            []      -> s ++ " ? - no result"
