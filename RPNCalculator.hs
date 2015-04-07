module RPNCalculator
where

push :: Int -> [Int] -> [Int]
push = (:)


err :: String -> String
err s = s ++ " - no result"
