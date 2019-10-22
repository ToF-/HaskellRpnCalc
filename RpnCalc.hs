module RpnCalc
where

type Stack = [Integer]
data Token = Const Integer
           | Unary (Integer -> Integer)

eval :: Stack -> Token -> Stack
eval st (Const n) = n : st
eval (n:ns) (Unary f) = f n : ns
