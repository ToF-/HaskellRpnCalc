import Data.Char
import Data.List

data Prefix = Number Integer
            | UnOp Char (Integer -> Integer) Prefix
            | BinOp Char (Integer -> Integer -> Integer) Prefix Prefix

instance Show Prefix where
    show (Number n) = show n
    show (UnOp c f p) = "(" ++ [c] ++ " " ++ (show p)++")"
    show (BinOp c f p q) = "(" ++ [c] ++ " " ++ (show p) ++ " " ++ (show q)++")"

type Parser = String -> [(Prefix ,String)]

prefix :: Parser 
prefix (' ':s) = prefix s
prefix (c:s) = case c `lookup` unOps of
    Just f -> let [(p,s')] = prefix s in [(UnOp c f p,s')]
    Nothing -> case c `lookup` binOps of
        Just f -> let
                    [(p,s')] = prefix s
                    [(q,s'')] = prefix s'
               in [(BinOp c f p q,s'')]
        Nothing -> number (c:s)
prefix _ = []
    

number :: Parser
number s = case reads s :: [(Integer,String)] of
    [] -> []
    [(n,s)] -> [(Number n,s)]

unOps = [('~', negate), ('!', factorial)]
binOps = [('+', (+)), ('-', flip (-)), ('*', (*)), ('/', flip div), ('%', flip mod)]

factorial n = product [1..n]
