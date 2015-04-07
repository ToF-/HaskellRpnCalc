module RPNCalculator
where

push :: Int -> [Int] -> Either String [Int]
push n st = Right (n:st)

err :: String -> [Int] -> Either String [Int]
err s _ = Left s

unary :: (Int -> Int) -> [Int] -> Either String [Int]
unary f (n:ns) = Right (f n:ns)

binary :: (Int -> Int -> Int) -> [Int] -> Either String [Int]
binary f (n:m:ns) = Right (f m n:ns)

