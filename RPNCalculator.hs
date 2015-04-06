module RPNCalculator (eval)
where
type Stack = Either String [Int]

eval :: String -> String
eval s = case foldl evalWord (Right []) $ words s of
            Right (n:ns) -> show n
            Left m       -> m

evalWord :: Stack -> String -> Stack
evalWord st s = case reads s :: [(Int,String)] of
    [(n,_)] -> push st n
    []      -> oper st s

push :: Stack -> Int -> Stack
push (Right ns) n = Right (n:ns)

oper :: Stack -> String -> Stack
oper st "neg" = unary negate st 
oper st "+"  = binary (+) st
oper st "*"  = binary (*) st
oper (Right _) s = Left (s ++ " ? - no result")

binary :: (Int -> Int -> Int) -> Stack -> Stack
binary f (Right (n:m:ns)) = Right (f n m:ns)

unary :: (Int -> Int) -> Stack -> Stack
unary f (Right (n:ns)) = Right (f n:ns)
