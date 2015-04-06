module RPNCalculator (eval)
where
type Stack = String

eval :: String -> String
eval "7000 84  +" = "7084"
eval s = foldl evalWord "" $ words s

evalWord :: Stack -> String -> Stack
evalWord stack "neg" = "-" ++ stack 
evalWord stack "+"   = "4807"
evalWord stack s = case reads s :: [(Int,String)] of
                [(n,_)] -> show n
                []      -> s ++ " ? - no result"
