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
calc = return []

push :: Int -> Stack -> Calculator
push n ns = Right $ n:ns


err :: String -> Stack -> Calculator
err s ns = Left $ s ++ " - no result"

unary :: (Int -> Int) -> Stack -> Calculator
unary f (n:ns) = push (f n) ns 

binary :: (Int -> Int -> Int) -> Stack -> Calculator
binary f (n:m:ns) = push (f m n) ns

inspect :: Int -> Stack -> Calculator
inspect 2 st@(n:m:ns) = return st 
inspect 1 st@(n:ns) = return st 
inspect _ st = err "not enough parameters" st
