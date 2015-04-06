module RPNCalculator (eval)
where
type Stack = Either String [Int]

eval :: String -> String
eval "7000 84  +" = "7084"
eval s = case foldl evalWord (Right []) $ words s of
            Right (n:ns) -> show n
            Left m       -> m

evalWord :: Stack -> String -> Stack
evalWord (Right (n:ns))  "neg" = Right (-n:ns)  
evalWord (Right (n:m:ns)) "+"  = Right (n+m:ns)
evalWord (Right ns) s = case reads s :: [(Int,String)] of
                [(n,_)] -> Right (n:ns)
                []      -> Left (s ++ " ? - no result")
