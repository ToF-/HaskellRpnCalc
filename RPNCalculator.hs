module RPNCalculator (Calculator, calculator, pop, push, binary, unary )
where
import Control.Monad

type Stack = [Double]
type Calculator = Either String Stack

calculator = Right []

result :: Calculator -> Either String Double 
result c = c >>= pop

pop :: Stack -> Either String Double
pop [] = Left "stack is empty - no result"
pop (n:_) = Right n

push :: Double -> Stack -> Calculator
push n ns = Right (n:ns)

binary :: (Double -> Double -> Double) -> Stack -> Calculator
binary f ns | length ns < 2  = Left "not enough parameters - no result"
binary (/) (0:_) = Left "division by zero - no result" 
binary f (n:m:ns) = Right ((f m n):ns)

unary :: (Double -> Double) -> Stack -> Calculator
unary f [] = Left "not enough parameters - no result" 
unary f (n:ns) = Right (f n :ns)



