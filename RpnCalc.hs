module RpnCalc
where

type Stack = [Integer]
data Token = Const Integer

eval :: Stack -> Token -> Stack
eval st (Const n) = n : st
