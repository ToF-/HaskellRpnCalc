module RpnCalc
where

type Stack = [Integer]
data Token = Const Integer

eval :: Stack -> Token -> Stack
eval _ (Const 42) = [42]
