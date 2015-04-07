module RPNCalculator
where

push :: Int -> [Int] -> Either String [Int]
push n st = Right (n:st)

err :: String -> [Int] -> Either String [Int]
err s _ = Left s
