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
Evaluating a prefix expression is easy because there is no surprise: the first element of any expression -- or sub expression -- always dictates how the rest of the expression should be interpreted. For instance in the expression $*+42\, 17\,!5$, from reading the $*$ symbol we know that we should find two operands in the rest of the expression. The first operand starts with the $+$ symbol, which indicates another binary operation, and so on. \\
Thus if we have determined each element of the expression and collected them into a list of \emph{tokens}, we can easily change that list into a tree: \\
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
Evaluating such a tree is straightforward. Let's start with that easy part.
\section{Evaluating a tree of tokens}
A token in a prefix expression that has been correctly parsed can be:
\begin{itemize}
\item A number,
\item An unary operation, like factorial, or negation,
\item A binary operation, like addition or multiplication.
\end{itemize}

\begin{code}
type Number = Integer
type Unary  = Number -> Number
type Binary = Number -> Number -> Number 
data Token = NumberTk Number
           | UnaryTk  Unary
           | BinaryTk Binary
\end{code}
A tree of tokens is formed by adding specific tokens to the tree.
\begin{code}
data Tree a = Nil
            | Node a (Tree a) (Tree a)

numberT :: Number -> Tree Token
numberT n                   = Node (NumberTk n) Nil Nil

unaryT :: Unary -> Tree Token -> Tree Token 
unaryT  f operand           = Node (UnaryTk f) operand Nil

binaryT :: Binary -> Tree Token -> Tree Token -> Tree Token
binaryT f operand1 operand2 = Node (BinaryTk f) operand1 operand2 

\end{code}
As an example the tree representing the expression \verb|*+42 17!5| should be equivalent to: following tree:
\begin{code}
fact n = product [1..n]
example = binaryT (*)
            (binaryT (+) 
                (numberT 42)
                (numberT 17))
            (unaryT fact
                (numberT 5))
\end{code}
When evaluating the tree for a prefix expression, we need to examine the token at the root of this tree, and use the subtrees depending on what sort of token it is.
\begin{code}
eval :: Tree Token -> Number
eval (Node (NumberTk n) _ _) = n
eval (Node (UnaryTk f) operand _)  = f (eval operand)
eval (Node (BinaryTk f) operand1 operand2) = f (eval operand1) (eval operand2)
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
    [(n,s)] | n >= 0 -> [(NumberTk n,s)]
            | otherwise -> []
\end{code}
Let's define some operators that our parsers will recognize.
\begin{code}
[sAdd,sSub,sMul,sDiv,sMod,sNeg,sFac] = "+-*/%-!"
\end{code}
To parse an unary operator, we need to recognize one of the symbols for such operators:
\begin{code}
unaryOp :: Parser Token
unaryOp (c:s) | c == sNeg = [(UnaryTk negate, s)]
unaryOp (c:s) | c == sFac = [(UnaryTk fact, s)]
unaryOp _ = []
\end{code}
The same logic applies to binary operators:
\begin{code}
binaryOp :: Parser Token
binaryOp (c:s) | c == sAdd = [(BinaryTk (+), s)]
binaryOp (c:s) | c == sSub = [(BinaryTk (-), s)]
binaryOp (c:s) | c == sMul = [(BinaryTk (*), s)]
binaryOp (c:s) | c == sDiv = [(BinaryTk div, s)]
binaryOp (c:s) | c == sMod = [(BinaryTk mod, s)]
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
tree  = 

number :: P= list (spaces num)
unary  = list (spaces unaryOp)
binary = list (spaces binaryOp)
\end{code}

We should be able to recognize a sequence of different tokens, so let's write a parser that is defined by the sequence of two parsers:
\begin{code}
seqP :: Parser [a] -> Parser [a] -> Parser [a]
seqP parserA parserB s = case parserA s of
    [] -> []
    rs -> [(a++b,u) 
          | (a,t) <- rs
          , (b,u) <- parserB t]
\end{code}
We also want to combine two parsers so that one or the other succeeds, so let's write a parser that is defined by the alternative of two parsers.
\begin{code}
altP :: Parser [a] -> Parser [a] -> Parser [a]
altP parserA parserB s = case parserA s of
    [] -> parserB s
    rs -> rs
\end{code}
Defining operators for these function will make the code more expressive.
\begin{code}
infixl 2 <|>
infixl 3 <.>

(<|>) = altP
(<.>) = seqP
\end{code}
Now we can define a parser for prefix expressions:
\begin{code}
expression :: Parser [Token]
expression = number 
          <|> binary <.> expression <.> expression 
          <|> unary <.> expression
\end{code}

Now we can define the main function to evaluate a prefix expression. If the expression has correctly been parsed, we evaluate the first list of tokens, and extract the number from the result.
\begin{code}
prefix :: String -> Number
prefix s = case expression s of
    [] -> error "incorrect prefix expression"
    ((ts,_):_) -> eval ts
\end{code}
Our main program consist in evaluating prefix expressions:
\begin{code}
main = interact $ unlines . map (show . prefix) . lines
\end{code}

\end{document}
