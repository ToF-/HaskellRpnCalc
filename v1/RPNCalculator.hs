module RPNCalculator (eval)
where
type Calculator = Either String Stack
type Stack  = [Int]

eval :: String -> String
eval s = case foldl evalWord (Right []) $ words s of
            Right (n:ns) -> show n
            Left m       -> m

evalWord :: Calculator -> String -> Calculator
evalWord st s = case reads s :: [(Int,String)] of
    [(n,_)] -> push st n
    []      -> oper s st

push :: Calculator -> Int -> Calculator
push (Left m) _ = Left m
push (Right ns) n = Right (n:ns)

oper :: String -> Calculator -> Calculator
oper "neg" st = unary negate st
oper "+"  st = binary st (+) 
oper "*"  st = binary st (*) 
oper "-"  st = binary st (-) 
oper "/"  st = binary st div 
oper s st = evalError (s ++ " ? - no result") st 

evalError :: String -> Calculator -> Calculator
evalError s (Left m) = Left m
evalError s _ = Left s

binary :: Calculator -> (Int -> Int -> Int) -> Calculator
binary (Left m) f = Left m
binary (Right [_]) f = Left "not enough parameters - no result"
binary (Right (n:m:ns)) f = Right (f m n:ns)

unary :: (Int -> Int) -> Calculator -> Calculator
unary f (Right []) = Left "not enough parameters - no result"
unary f (Right (n:ns)) = Right (f n:ns)

inspect :: Int -> Calculator -> Calculator
inspect 1 (Right []) = Left "not enough parameters - no result"
inspect 2 (Right [_])= Left "not enough parameters - no result"
inspect _ st = st
