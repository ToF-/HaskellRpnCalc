\documentclass[a4paper,10pt]{article}
\usepackage{longtable,geometry}
\usepackage{tabularx}
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
\usepackage[linguistics]{forest}
\usepackage{wallpaper}

%include lhs2TeX.fmt
%include lhs2TeX.sty
%include spacing.fmt
%include polycode.fmt
\newcommand\tab[1][1cm]{\hspace*{#1}}
\begin{document}
\setlength{\parindent}{0em}
\section{Let's write a program...}
Let's write a program that reads expressions in prefix notation, and writes their value as an output. Here are examples of such expressions:\\

\begin{center}
\begin{tabular}{c c c}
\emph{prefix expression} & \emph{value} & \emph{infix equivalent}\\
\hline 
\hline
\verb|*+42 17!5 | & $7080$ & $(42+17)\times!5$ \\
\verb|+4-3 +10-5| & $2$ & $(4+3-(10+(-5)))$ \\
\hline
\end{tabular}\\
\end{center}
Note how the \verb|-| symbol can be interpreted as the minus sign or the subtraction operator.

\begin{code}
module Prefix
where
\end{code}
To calculate the value of an expression such as:\\
\begin{center}
$*\,+\,42\,17\,!5$
\end{center}
we need to collect its \emph{tokens} ($*$,\,$+$,\,$42$,\,$17$,\,$!$,\,$5$) in a \emph{synctatic tree}. 
\begin{center}
\begin{forest}
    [$*$
        [$+$
            [$42$]
            [$17$]
        ]
        [$!$
            [$5$]
        ]
    ]
\end{forest}
\end{center}
Then we can evaluate this tree according to three rules:
\begin{itemize}
\item a tree node containing a number evaluates to this number,
\item a tree node containing an unary operator applies this operator to its first subtree,
\item a tree node containing a binary operator applies this operator to its first and second subtrees.
\end{itemize}
\section{Some data types}
We we have to deal with values and functions of arity 1 and 2.
\begin{code}
type Value = Integer
type Unary  = Value -> Value
type Binary = Value -> Value -> Value 
\end{code}
A token can either be a numeric value, an unary operator or a binary operator.
\begin{code}
data Token = V Value
           | U Unary
           | B Binary
\end{code}
A tree can contain 0, 1 or 2 subtrees.
\begin{code}
data Tree a = Nil 
            | Node a (Tree a) (Tree a)
\end{code}
A parser of type $a$ is a function from $String$ to a list of couples $[(a,String)]$. An empty list means that parsing the string let to no result, and a list with several couples 

\end{document}

When evaluating the tree for a prefix expression, we need to examine the token at the root of this tree, and use the subtrees depending on what sort of token it is.
\begin{code}
eval :: Tree Token -> Value
eval (Node (V n) _ _) = n
eval (Node (U f) operand _)  = f (eval operand)
eval (Node (B f) operand1 operand2) = f (eval operand1) (eval operand2)
\end{code}
Thus the expression \verb|fst (eval example)| should yield $7080$.\\
\section{Parsing a prefix expression}
A parser is a function that scans a string and recognizes a token, or a given pattern. The result of the parsing is a list of tuples \verb|(a,String)|, since there can be several distinct results from parsing a string.
\begin{code}
type Parser a = String -> [(a,String)]
\end{code}
Let's first parse numbers, using the \verb|reads| parser already present in Haskell prelude. We must avoid parsing negative numbers, because althoug our notation allows for the minus sign to indicate either a binary subtraction or the unary negation, we want the symbol to be interpreted by our parser, not being consumed by \verb|reads|.
\begin{code}
num :: Parser Token
num s = case reads s of
    [] -> []
    [(n,s)] | n >= 0 -> [(V n,s)]
            | otherwise -> []
\end{code}
Let's define some operators that our parsers will recognize.
\begin{code}
[sAdd,sSub,sMul,sDiv,sMod,sNeg,sFac] = "+-*/%-!"
\end{code}
To parse an unary operator, we need to recognize one of the symbols for such operators:
\begin{code}
fact n = product [1..n]

unaryOp :: Parser Token
unaryOp (c:s) | c == sNeg = [(U negate, s)]
unaryOp (c:s) | c == sFac = [(U fact, s)]
unaryOp _ = []
\end{code}
The same logic applies to binary operators:
\begin{code}
binaryOp :: Parser Token
binaryOp (c:s) | c == sAdd = [(B (+), s)]
binaryOp (c:s) | c == sSub = [(B (-), s)]
binaryOp (c:s) | c == sMul = [(B (*), s)]
binaryOp (c:s) | c == sDiv = [(B div, s)]
binaryOp (c:s) | c == sMod = [(B mod, s)]
binaryOp _ = []
\end{code}
Since spaces can separate numbers from operators, we need a function to augment our parsers so that they consume spaces before recognizing tokens.
\begin{code}
spaces :: Parser a -> Parser a 
spaces p (' ':s) = spaces p s
spaces p s = p s
\end{code}
We know that expressions are formed with trees of tokens, so we need functions that will use our basic parsers and put their result into a tree.
\begin{code}
tree :: Parser a -> Parser (Tree a)
tree p s = case p s of
    [] -> []
    [(a,s)] -> [(Node a Nil Nil,s)]

number = tree (spaces num)
unary  = tree (spaces unaryOp)
binary = tree (spaces binaryOp)
\end{code}

We should be able to recognize a sequence of different tokens, so let's write a parser that is defined by the sequence of two parsers:
\begin{code}
seqP :: Parser (Tree a) -> Parser (Tree a) -> Parser (Tree a)
seqP parserA parserB s = case parserA s of
    [] -> []
    rs -> [(grow a b,u) 
          | (a,t) <- rs
          , (b,u) <- parserB t]
        where
        grow :: Tree a -> Tree a -> Tree a
        grow Nil t = t
        grow (Node a Nil Nil) t = Node a t Nil
        grow (Node a l Nil)   t = Node a l t
\end{code}
We also want to combine two parsers so that one or the other succeeds, so let's write a parser that is defined by the alternative of two parsers.
\begin{code}
altP :: Parser (Tree a) -> Parser (Tree a) -> Parser (Tree a)
altP parserA parserB s = case parserA s of
    [] -> parserB s
    rs -> rs
\end{code}
Defining operators for these function will make the code more expressive.
\begin{code}
infixl 2 <|>
infixl 3 <&>

(<|>) = altP
(<&>) = seqP
\end{code}
Now we can define a parser for prefix expressions:
\begin{code}
expression :: Parser (Tree Token)
expression = number 
          <|> unary <&> expression
          <|> binary <&> expression <&> expression 
\end{code}

Now we can define the main function to evaluate a prefix expression. If the expression has correctly been parsed, we evaluate the first list of tokens, and extract the number from the result.
\begin{code}
prefix :: String -> Value
prefix s = case expression s of
    [] -> error "incorrect prefix expression"
    ((ts,_):_) -> eval ts
\end{code}
Our main program consist in evaluating prefix expressions:
\begin{code}
main = interact $ unlines . map (show . prefix) . lines
\end{code}

\end{document}
