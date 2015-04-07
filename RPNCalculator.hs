module RPNCalculator
where

type Stack = [Int]
type Calculator = Either String Stack

{-- remember that:

instance Monad (Either a) where
    return x = Right x
    val >>= f = case val of
        Left err -> Left err
        Right x -> f x
--}

calc :: Calculator
calc = Right []

push :: Int -> Stack -> Calculator
push n ns = Right $ n:ns


err :: String -> Stack -> Calculator
err s ns = Left $ s ++ " - no result"

unary :: (Int -> Int) -> Stack -> Calculator
unary f (n:ns) = push (f n) ns 
