module RPNCalculator
where

type Stack = [Int]
type Calculator = Either String Stack

push :: Int -> Stack -> Calculator
push n ns = Right $ n:ns


err :: String -> Stack -> Calculator
err s ns = Left $ s ++ " - no result"

