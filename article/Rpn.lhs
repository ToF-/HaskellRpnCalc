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
import Data.List (break)
import Data.Char (isDigit, isSpace)
\end{code}
The calculator is given a @String@ containing an RPN expression, and yields the value of this expression, a @Number@. Here we choose @Number@ to be equivalent to @Integer@, but another numeric type would also work.
\begin{code}
type Number = Integer
rpn :: String -> Number
rpn = undefined
\end{code}
The meaning of an RPN expression can be expressed as a list of \emph{tokens}. A token is either a value or an operator. Most operators operate on two numbers like @+@ or @*@. Some of the operators are \emph{unary} operators, like @!@. Let's describe this in Haskell:
\begin{code}
data Token = Val Number 
           | Op1  (Number -> Number)
           | Op2 (Number -> Number -> Number)
\end{code}
Since we cannot @show@ a value of type @Number -> Number@, let's define a specific "evasive" way of showing a token:

\begin{code}
instance Show Token where
    show (Val n) = show n
    show (Op1 _) = "<op1>"
    show (Op2 _) = "<op2>"
\end{code}

To evaluate the expression, we execute each token on a \emph{stack}, a structure that allows for inserting and removing numbers in a \emph{last in, first out} fashion. A stack can either be empty (in which case applying an operator should result in a program error) or have an item followed by a possibly empty sequence of item previously entered on the stack.
\begin{code}
data Stack = Empty
           | Item Number Stack
    deriving (Show)
\end{code}
We can create an empty stack, and also tell if a stack is empty.
\begin{code}
empty :: Stack
empty = Empty

isEmpty :: Stack -> Bool
isEmpty Empty = True
isEmprt _     = False
\end{code}
@push@, takes a number, an existing stack, and puts that number on top of the existing stack, thus yielding a new stack.
\begin{code}
push :: Number -> Stack -> Stack
push = Item 
\end{code}
@pop@, extracts the number at the top and returns this number and the stack without this number. Popping a number from the empty stack results in a progam error. 
\begin{code}
pop :: Stack -> (Number, Stack)
pop (Item n st) = (n,st)
pop Empty = error "empty stack"
\end{code}
Here are two functions to convert the content of a stack into a list of numbers and vice versa.
\begin{code}
toList :: Stack -> [Number]
toList = reverse . toList'
    where
    toList' Empty = []
    toList' st = n : toList' st' 
        where (n,st') = pop st

fromList :: [Number] -> Stack
fromList  = foldl (flip push) Empty 
\end{code}
Applying the token @Number @ $n$ on the stack results in pushing the integer $n$ on the top op the stack. Applying an unary operator consists in applying the operator on the top of the stack, while applying a binary operator is done by pulling two values from the stack, then pushing the result of the operation. 
\begin{code}
apply :: Stack -> Token -> Stack
apply st (Val n) = Item n st
apply st (Op1 op) = Item (op n) st' 
    where (n,st') = pop st
apply st (Op2 op) = Item (m `op` n) st''
    where
        (n,st')  = pop st
        (m,st'') = pop st'
\end{code}
We can now evaluate expressions when conveyed as a list of RPN tokens. For example apply the following tokens to a stack containing a temperature value will convert this value from Celsius to Fahreinheit ($ F = \frac{9C}{5} + 32$) : @[Val 9, Op2 (*), Val 5, Op2 div, Val 32, Op2 (+)]@\\
The remaining problem is to translate a string representing an RPN expression into a list of tokens. This is the work of a @rpnParser@, which itself is a combination of simpler parsers. \\
\begin{code}
type Parser = String -> ([Token],String)
\end{code}
The principle of a parser is to recognize a given pattern in a given string, and return a list of tokens and the part of the string that remains to be parsed. If the pattern is present in the input string, the list contains the tokens that were recognized, if not, the list is empty, and the remaining string is the same as the input string. 

For example a parser for the operator @!@ recognize only that character. Every other string value will result in a empty list of tokens.
\begin{code}
pFactorial :: Parser
pFactorial ('!':cs) = ([Op1 (\n -> product [1..n])],cs)
pFactorial s = ([],s)
\end{code}
Instead of repeating this form for every operator, we can create a "factory of parser" : 

\begin{code}
pOperatorFor :: Char -> Token -> Parser
pOperatorFor o t = \s -> case s of 
    (c:cs) | c == o -> ([t],cs)
    _              -> ([],s) 
\end{code}
We can combine parsers in a way such that the parsed expression is recognized if contains one pattern or the other. 
\begin{code}
infixl <|>
(<|>) :: Parser -> Parser -> Parser
parserA <|> parserB = \s -> 
    let  
        (ta,sA) = parserA s
    in case null ta of
            False -> (ta,sA)
            True -> parserB sA
\end{code}

Then a parser recognizing any of our operators can be created by folding this 'or' on a list mapping each char with its token:
\begin{code}
operators :: [(Char,Token)]
operators = [('+',Op2 (+)), ('*',Op2 (*)),('-',Op2 (-)),('/',Op2 div),('%', Op2 mod)
             ,('~',Op1 negate),('!', Op1 (\n -> product [1..n]))]

pOperator :: Parser
pOperator = foldl1 (<|>) (map (uncurry pOperatorFor) operators)
\end{code}
Parsing a number is simple: extract all characters that are digits, and produce the @Val@ token, and the remaining part of the input string, or if there are no digits, return the empty list and the unchanged input string.
\begin{code}
number :: Parser
number s = case break (not . isDigit) s of
    ("", _) -> ([],s)
    (digits,rest) -> ([Val (read digits)],rest)
\end{code}
Spaces in an RPN expression should be ignored. Thus the parser for space doesn't yield a specific token, it only trims the remaining string.

\begin{code}
space :: Parser
space s = ([],s')
    where (_,s') = break (not . isSpace) s 
\end{code}
To parse an operator, take the first character of the input string and look it up in a list of all of operators, yielding the matching token.
\begin{code}
parseOperator :: Parser
parseOperator "" = ([],"")
parseOperator s@(c:cs) = case lookup c operators of
    Nothing -> ([],s)
    Just op -> ([op],cs)

\end{code}

\end{document}
