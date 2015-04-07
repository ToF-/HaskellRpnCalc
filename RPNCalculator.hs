module RPNCalculator
where

type Stack = [Int]
type Calculator = Either String Stack

calc :: Calculator
calc = Right []

push :: Int -> Stack -> Calculator
push n ns = Right $ n:ns


err :: String -> Stack -> Calculator
err s ns = Left $ s ++ " - no result"

