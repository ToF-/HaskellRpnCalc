module RPNCalculator
where
type Stack = [Int]
type Calculator = Either String Stack

calc :: Calculator
calc = Right []

push :: Int -> Stack -> Calculator
push n st = Right (n:st)

err :: String -> Stack -> Calculator
err s _ = Left s

unary :: (Int -> Int) -> Stack -> Calculator
unary f st = inspect 1 st >>= safeUnary f 

binary :: (Int -> Int -> Int) -> Stack -> Calculator
binary f st = inspect 2 st >>= safeBinary f 

inspect :: Int -> Stack -> Calculator
inspect 1 (n:ns) = Right (n:ns)
inspect 2 (n:m:ns) = Right (n:m:ns)
inspect _ _ = Left "not enough parameters - no result"

safeUnary :: (Int -> Int) -> Stack -> Calculator
safeUnary f (n:ns) = Right (f n:ns)

safeBinary :: (Int -> Int -> Int) -> Stack -> Calculator
safeBinary div (0:_) = Left "division by zero - no result"
safeBinary f (n:m:ns) = Right (f m n:ns)

cmd :: String -> Stack -> Calculator
cmd "neg" = unary negate 
cmd "+"   = binary (+)

