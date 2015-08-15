module Calc
where
import Data.List (groupBy)
import Data.Char (isDigit)

type Number = Int
type Stack = [Number]
type Result = Either String Number
type Calc = Either String Stack
type Operation = Stack -> Calc

main = interact (unlines . rpn . lines)

rpn :: [String] -> [String]
rpn = map rpnline . tail . (scanl calc' (Right []))

rpnline :: Calc -> String 
rpnline c = case c of
    Right (n:ns) -> show n
    Right [] -> ""
    Left  s -> s

calc :: String -> Result
calc s = (calc' (Right []) s) >>= return . head

calc' :: Calc -> String -> Calc
calc' c = (foldl eval initial . groupBy digits)
    where initial = case c of 
            Left _ -> Right []
            _      -> c


digits c c' = isDigit c && isDigit c'
         

push :: Number -> Operation
push n = return . (n:) 

unary :: (Number -> Number) -> Operation
unary f st = do (n,st') <- pull st
                push (f n) st'

binary :: (Number -> Number -> Number) -> Operation
binary f st = do (n,st') <- pull st
                 unary (f n) st'

pull :: Stack -> Either String (Number,Stack)
pull []     = Left "not enough parameters"
pull (n:ns) = Right (n,ns)

eval :: Calc -> String -> Calc
eval c s = c >>= (parse s)

parse :: String -> Operation
parse "~" =  unary negate
parse "!" =  unary (\x -> product [1..x])
parse " " =  return . id
parse "+" =  binary (+)
parse "*" =  binary (*)
parse "-" =  binary (flip (-))
parse "/" =  nonZero (binary (flip div))
parse "%" =  nonZero (binary (flip mod))
parse s   = case reads s of
    []      ->  return $ Left $ s ++ " ??"
    [(n,_)] ->  push n

nonZero :: Operation -> Operation
nonZero f (0:ns) = Left "division by zero"
nonZero f st     = Right st >>= f

    
