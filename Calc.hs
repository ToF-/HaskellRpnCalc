module Calc
where
    
type Calc = Either Message Number
type Number = Int
type Message = String

calc :: String -> String
calc = result . foldl (>>=) initial . map parse . words
    where initial = Right 0
          result (Right n) = show n
          result (Left m)  = m

parse :: String -> Number -> Calc
parse s = case reads s :: [(Number,String)] of
    [(n,_)] -> Right . const n
    []      -> Left  . const (s ++ " ??")
    
