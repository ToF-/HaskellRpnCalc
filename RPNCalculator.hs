module RPNCalculator (eval)
where

eval :: String -> String
eval s = foldl evalWord "" $ words s

evalWord :: String -> String -> String
evalWord top "neg" = "-" ++ top 
evalWord top s = case reads s :: [(Int,String)] of
                [(n,_)] -> show n
                []      -> s ++ " ? - no result"
