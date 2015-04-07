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
unary f (n:ns) = Right (f n:ns)

binary :: (Int -> Int -> Int) -> Stack -> Calculator
binary f (n:m:ns) = Right (f m n:ns)

