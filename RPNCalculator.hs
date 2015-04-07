module RPNCalculator
where

push :: Int -> [Int] -> Either String [Int]
push n ns = Right $ n:ns


err :: String -> [Int] -> Either String [Int]
err s ns = Left $ s ++ " - no result"

