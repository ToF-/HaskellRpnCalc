module Calc
where
    
type Calc = Either Message Number
type Number = Int
type Message = String

calc :: String -> String
calc = last . foldl eval initial .  words
    where initial = Right 0
          last (Right n) = show n
          last (Left m)  = m

eval :: Calc -> String -> Calc
eval (Left m) _ = Left m
eval (Right _) s = case reads s of
    [(n,_)] -> Right n
    []      -> Left $ s ++ " ??"
