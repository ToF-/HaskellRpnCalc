module Calc
where

type Number = Int

calc :: String -> String
calc s = case reads s :: [(Number,String)] of
    [(n,_)] -> show n
    []      -> s ++ " ??"
