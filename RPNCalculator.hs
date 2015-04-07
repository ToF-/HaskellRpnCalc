module RPNCalculator
where

push :: Int -> [Int] -> Either String [Int]
push n st = Right (n:st)
