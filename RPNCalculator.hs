module RPNCalculator (eval)
where

eval :: String -> String
eval s = case reads s :: [(Int,String)] of
            [(n,_)] -> show n
            []      -> s ++ " ? - no result"
