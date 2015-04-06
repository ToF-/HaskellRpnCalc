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
    []      -> oper s st

push :: Stack -> Int -> Stack
push (Right ns) n = Right (n:ns)

oper :: String -> Stack -> Stack
oper "neg" = unary negate  
oper "+"  = binary (+) 
oper "*"  = binary (*) 
oper "-"  = binary (-)
oper "/"  = binary div
oper s = evalError (s ++ " ? - no result")

evalError :: String -> Stack -> Stack
evalError s _ = Left s

binary :: (Int -> Int -> Int) -> Stack -> Stack
binary f (Right [_]) = Left "not enough parameters - no result"
binary f (Right (n:m:ns)) = Right (f m n:ns)

unary :: (Int -> Int) -> Stack -> Stack
unary f (Right []) = Left "not enough parameters - no result"
unary f (Right (n:ns)) = Right (f n:ns)
