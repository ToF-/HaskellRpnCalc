module Calc
where

type Number = Int

calc :: String -> String
calc = last . map eval .  words

eval :: String -> String
eval s = case reads s :: [(Number,String)] of
    [(n,_)] -> show n
    []      -> s ++ " ??"
