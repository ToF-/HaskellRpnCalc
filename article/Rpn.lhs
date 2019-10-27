\documentclass{article}

%include lhs2TeX.fmt
%include lhs2TeX.sty
%include spacing.fmt

\begin{document}
\setlength{\parindent}{0em}
This is the module for the reverse polish notation calculator. 
\begin{code}
module Rpn 
where
\end{code}
The calculator is given a @String@ containing an RPN expression, and yields the value of this expression, a @Number@. Here we choose @Number@ to be equivalent to @Integer@, but another numeric type would also work.
\begin{code}
type Number = Integer
rpn :: String -> Number
rpn = undefined
\end{code}
The meaning of an RPN expression can be expressed as a list of \emph{tokens}. A token is either a value or an operator. Most operators operate on two numbers like @+@ or @*@:they are called \emph{binary} operators, and some of them are \emph{unary} operators, like @!@. Let's describe this in Haskell:
\begin{code}
data Token = Value Number 
           | Unary  (Number -> Number)
           | Binary (Number -> Number -> Number)
\end{code}
To evaluate the expression, we execute each token on a \emph{stack}, a structure that allows for inserting and removing numbers in a \emph{last in, first out} fashion. A stack can either be empty (in which case applying an operator should result in a program error) or have an item followed by a possibly empty sequence of item previously entered on the stack.
\begin{code}
data Stack = Empty
           | Item Number Stack
    deriving (Show)
\end{code}
Two functions are defined that use a @Stack@. The first one, @push@, takes an existing stack, a number, and puts that number on top of the existing stack, thus yielding a new stack.
\begin{code}
push :: Stack -> Number -> Stack
push st n = Item n st
\end{code}
The second one, @pop@, extracts the number at the top and returns this number and the stack without this number. Popping a number from the empty stack results in a progam error. 
\begin{code}
pop :: Stack -> (Number, Stack)
pop (Item n st) = (n,st)
pop Empty = error "empty stack"
\end{code}
(Here's some function to present the content of a stack as a list of numbers and vice versa)
\begin{code}
toList :: Stack -> [Number]
toList = reverse . toList'
    where
    toList' Empty = []
    toList' st = n : toList' st' where (n,st') = pop st

fromList :: [Number] -> Stack
fromList  = foldl push Empty 
\end{code}
Applying the token @Number @ $n$ on the stack results in pushing the integer $n$ on the top op the stack. Applying an unary operator consists in applying the operator on the top of the stack, while applying a binary operator is done by pulling two values from the stack, then pushing the result of the operation. 
\begin{code}
apply :: Stack -> Token -> Stack
apply st (Value n) = Item n st
apply st (Unary op) = let (n, st')    = pop st in Item (op n) st'
apply st (Binary op) =
    let (n, st') = pop st
        (n', st'') = pop st'
    in Item (n' `op` n) st'' 
\end{code}
We can now calculte values expressed as a list of RPN tokens: for example the following list will convert a temperature from Celsius to Fahreinheit:
\begin{code}
cToF :: Number -> Number
cToF n = head $ toList $ foldl apply (fromList [n]) 
    [Value 9
    , Binary (*)
    , Value 5
    , Binary div
    , Value 32
    , Binary (+)]
\end{code}
The remaining problem is to parse a string into a list of tokens.
\end{document}
