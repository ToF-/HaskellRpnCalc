module RPNCalculator (Calculator, calculator, pop, push, binary, unary, eval, output, process)
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

message :: String -> Stack -> Calculator
message s _ = Left s

eval :: String -> Either String Double
eval ss = (foldM (flip readOper) [] $ words ss) >>= pop
    where
    readOper :: String -> Stack -> Calculator
    readOper "neg" = unary negate
    readOper "*"   = binary (*)
    readOper "/"   = binary (/)
    readOper "+"   = binary (+)
    readOper "-"   = binary (-)
    readOper op    = case reads op of
                        [(d,_)] -> push d
                        []      -> message (op ++ " ? - no result") 

output :: Either String Double -> String
output (Right n) = show n
output (Left s)  = s

process :: String -> String
process = unlines . map (output . eval) . lines
