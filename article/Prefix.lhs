\documentclass[a4paper,10pt]{article}
\usepackage{longtable,geometry}
\usepackage[english]{babel}
\usepackage[latin1]{inputenc}
\usepackage[babel]{csquotes}
\usepackage{multicol}
\usepackage{enumitem}
\usepackage{array}
\usepackage{fancyhdr}
\pagestyle{plain}
\geometry{dvips,a4paper,margin=1.5in}
\usepackage[usenames,dvipsnames]{xcolor}
\usepackage{amssymb,amsmath} 
\usepackage{afterpage}
\usepackage{pagecolor}
\usepackage{fancybox}
\usepackage{listings}
\usepackage{verbatim}
\usepackage{fancyvrb}
\usepackage{moreverb}
\usepackage{txfonts}
\usepackage{hyperref}
\usepackage{bytefield}
\usepackage{rotating}
\usepackage{tikz}
\usepackage{wallpaper}

%include lhs2TeX.fmt
%include lhs2TeX.sty
%include spacing.fmt
%include polycode.fmt
\newcommand\tab[1][1cm]{\hspace*{#1}}
\begin{document}
\setlength{\parindent}{0em}
Let's write a parser for expressions in prefix notation. Here are examples:\\

\verb|*+42 17!5 | \tab $\rightarrow \tab (42+17)\times!5$ \\
\verb|+4-3 +10-5| \tab $\rightarrow \tab (4+3-(10+(-5)))$ \\

We will need built-in functions to detect a space or a digit character, so let's import these. 
\begin{code}
module Prefix
where
import Data.Char (isSpace, isDigit, digitToInt)
\end{code}

Evaluating a prefix expression requires two steps:
\begin{itemize}
\item parsing the expression into \emph{tokens},
\item evaluating these tokens according to the rules of the prefix notation.
\end{itemize}

\section{Tokens, and how to evaluate them}

Let's define the possible tokens for our prefix notation. A token can be:
\begin{itemize}
\item A number,
\item An operator representing an unary function (e.g. factorial)
\item An operator representing a binary function (e.g. multiplication)
\end{itemize}

\begin{code}
type Number = Integer
data Token = Num Number
           | Op1 (Number -> Number) 
           | Op2 (Number -> Number -> Number)
\end{code}
Since an unary operator should be followed by another expression, and a binary operator by two expressions, it is natural to represent a parsed expressionas as a list of tokens.
For example parsing the expression \verb|*+42 17!5| should result in the following list:
\begin{code}
example = [Op2 (*) ,Op2 (+) ,Num 42 ,Num 17 ,Op1 (\n->product[1..n]) ,Num 5]
\end{code}
To evaluate a list of tokens representing a prefix expression, we need to examine the token at the head of the list. If this token matches the pattern \verb|Num n|, then the value is $n$ and the rest of the list is to be evaluated further.\\
If the head of the list matches an unary operator, \verb|Op1 f|, we have to apply the function $f$ to the value represented by the rest of the list. \\
If the head of the list matches a binary operator, \verb|Op2 f|, then we have to first evaluate the rest of the list, which will give us the first operand value $n$ and a remainging list, and then the evaluation amounts to evaluating a list starting with the (unary) partial application $f n$ to the value given by the rest of the list. \\
Finally, evaluating an empty list should yield the (arbitrary) value $0$, and an empty list.
\begin{code}
eval :: [Token] -> (Number,[Token])
eval [] = (0, [])
eval (Num n:ts) = (n, ts)
eval (Op1 f:ts) = (f n,ts') where (n,ts') = eval ts
eval (Op2 f:ts) = eval (Op1 (f n):ts') where (n,ts') = eval ts
\end{code}
Thus the expression \verb|fst (eval example)| should yield $7080$.\\
Here's how the expression \verb|eval [Op2 (+),Num 42, Num 17]| is evaluated:\\
\begin{verbatim}
eval [Op2 (+),Num 42, Num 17]
eval (Op1 ((+) n):ts') where (n,ts') = eval [Num 42,Num 17]
eval (Op1 ((+) n):ts') where (n,ts') = (42, [Num 17])
eval (Op1 ((+) 42):[Num 17]) 
((+) 42 n,ts') where (n,ts') = eval [Num 17]
((+) 42 n,ts') where (n,ts') = (17,[])
((+) 42 17,[]) 
(59,[])
\end{verbatim}
\section{Parsing a prefix expression}
A parser is a function that scans a string and recognizes a token, or a sequence of tokens, or just anything else.
\begin{code}
newtype Parser a = Parser { parse :: String -> [(a,String)] }
\end{code}
We can parse a digit, converting it to its number value:
\begin{code}
digit :: Parser Number
digit = Parser $ \s -> case s of
    [] -> []
    (c:cs) | isDigit c -> [(toNumber c,cs)]
           | otherwise -> []
    where
    toNumber = fromIntegral . digitToInt
\end{code}
If we define the class of our parser as a monad, we can then chain effects on the result of a parser:

\begin{code}
instance Functor Parser where
    fmap f p = Parser $ \s -> [(f x,s') | (x,s') <- parse p s]
instance Applicative Parser
instance Monad Parser where
    m >>= k = Parser $ \s -> [(y,s'') | (x,s') <- parse m s, (y,s'') <- parse (k x) s'] 
\end{code}
\end{document}
