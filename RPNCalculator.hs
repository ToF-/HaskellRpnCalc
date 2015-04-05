module RPNCalculator (calculator, result, (-:), Operation (..))
where

type Stack = [Double]
data Operation = Number Double
               | Unary (Double -> Double)
               | Binary (Double -> Double -> Double)

type Calculator = Either String Stack

infixl 5 -:

calculator = Right []

result :: Calculator -> Either String Double 
result (Left m) = Left m
result (Right []) = Left "stack is empty - no result"
result (Right (n:_)) = Right n

(-:) :: Calculator -> Operation -> Calculator
(-:) (Left m) _ = Left m
(-:) (Right ns) (Number n) = Right (n:ns) 
(-:) (Right ([])) (Unary _) = Left "not enough parameters - no result" 
(-:) (Right (n:[])) (Binary _) = Left "not enough parameters - no result" 
(-:) (Right (n:ns)) (Unary op) = Right (op n:ns)
(-:) (Right (n:m:ns)) (Binary op) = Right (op n m:ns)

