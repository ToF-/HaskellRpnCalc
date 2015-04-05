module RPNCalculator (calculator, result, (-:), Operation (..))
where

type Calculator = [Double]
data Operation = Number Double
               | Binary (Double -> Double -> Double)

infixl 5 -:

calculator = []

result :: Calculator -> Either String Double 
result [] = Left "stack is empty - no result"
result (n:_) = Right n

(-:) :: Calculator -> Operation -> Calculator
(-:) c (Number n) = n:c 
(-:) (n:m:ns) (Binary (+)) = n+m:ns

