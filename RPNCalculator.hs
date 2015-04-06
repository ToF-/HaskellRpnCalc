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
push (Left m) _ = Left m
push (Right ns) n = Right (n:ns)

oper :: String -> Stack -> Stack
oper "neg" st = unary negate st
oper "+"  st = binary st (+) 
oper "*"  st = binary st (*) 
oper "-"  st = binary st (-) 
oper "/"  st = binary st div 
oper s st = evalError (s ++ " ? - no result") st 

evalError :: String -> Stack -> Stack
evalError s (Left m) = Left m
evalError s _ = Left s

binary :: Stack -> (Int -> Int -> Int) -> Stack
binary (Left m) f = Left m
binary (Right [_]) f = Left "not enough parameters - no result"
binary (Right (n:m:ns)) f = Right (f m n:ns)

unary :: (Int -> Int) -> Stack -> Stack
unary f (Right []) = Left "not enough parameters - no result"
unary f (Right (n:ns)) = Right (f n:ns)

inspect :: Int -> Stack -> Stack
inspect 1 (Right []) = Left "not enough parameters - no result"
inspect 2 (Right [_])= Left "not enough parameters - no result"
inspect _ st = st
