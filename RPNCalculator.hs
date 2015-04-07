module RPNCalculator
where
import Control.Monad (foldM)

type Stack = [Int]
type Calculator = Either String Stack

calc :: Calculator
calc = Right []

clear :: Stack -> Calculator 
clear _ = calc

push :: Int -> Stack -> Calculator
push n st = Right (n:st)

err :: String -> Stack -> Calculator
err s _ = Left (s ++ " - no result")

unary :: (Int -> Int) -> Stack -> Calculator
unary f st = inspect 1 st >>= safeUnary f 

binary :: (Int -> Int -> Int) -> Stack -> Calculator
binary f st = inspect 2 st >>= safeBinary f 

inspect :: Int -> Stack -> Calculator
inspect 1 (n:ns) = Right (n:ns)
inspect 2 (n:m:ns) = Right (n:m:ns)
inspect _ st = err "not enough parameters" st

safeUnary :: (Int -> Int) -> Stack -> Calculator
safeUnary f (n:ns) = Right (f n:ns)

safeBinary :: (Int -> Int -> Int) -> Stack -> Calculator
safeBinary div (0:ns) = err "division by zero" (0:ns)
safeBinary f (n:m:ns) = Right (f m n:ns)

cmd :: String -> Stack -> Calculator
cmd "clear" = clear
cmd "neg" = unary negate 
cmd "+"   = binary (+)
cmd "-"   = binary (-)
cmd "*"   = binary (*)
cmd "/"   = binary div
cmd s     = case reads s :: [(Int,String)] of
                [(n,_)] -> push n
                []      -> err (s ++ " ?")

eval :: String -> Stack -> Calculator
eval s st = foldM (flip cmd) st $ words s

process :: Calculator -> [String] -> [String]
process c [] = []
process c (s:ss) = 
    let c' = c >>= eval s
    in show c' : process c' ss    

