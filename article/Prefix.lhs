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
\item An operator for an unary function (e.g. factorial)
\item An operator for a binary function (e.g. multiplication)
\end{itemize}

\begin{code}
type Number = Integer
data Token = Num Number
           | Op1 (Number -> Number) 
           | Op2 (Number -> Number -> Number)
\end{code}
For the sake of evaluation, expressions can be seen as trees of tokens.
\begin{code}
data Tree a = Nil
            | Node a (Tree a) (Tree a)

numNode :: Number -> Tree Token
numNode n = Node (Num n) Nil Nil

op1Node :: (Number -> Number) -> Tree Token -> Tree Token
op1Node f t = Node (Op1 f) t Nil

op2Node :: (Number -> Number -> Number) -> Tree Token -> Tree Token -> Tree Token
op2Node f t u = Node (Op2 f) t u

\end{code}
For example parsing the expression \verb|*+42 17!5| should result in the following tree:
\begin{code}
fact n = product [1..n]
example = op2Node (*)
            (op2Node (+) 
                (numNode 42)
                (numNode 17))
            (op1Node fact
                (numNode 5))
\end{code}
To evaluate a tree of tokens representing a prefix expression, we need to examine the token at the root of three. \\
If this token matches the pattern \verb|Num n|, then the value is $n$ and the rest of the list is to be evaluated further.\\
\begin{code}
eval :: Tree Token -> Number
eval (Node (Num n) _ _) = n
\end{code}
If the root is a node that matches an unary operator, \verb|Op1 f|, we have to apply the function $f$ to the value represented by the first branch of this node. \\
\begin{code}
eval (Node (Op1 f) t _) = f (eval t)
\end{code}
If the node matches a binary operator, \verb|Op2 f|, we have to first evaluate the left and right subtrees, and then apply the finary function to these values.
\\
\begin{code}
eval (Node (Op2 f) t u) = f (eval t) (eval u)
\end{code}
Finally, evaluating any other form of tree should never happen (since it means that the input couldn't be parsed in a correct tree of tokens).
\begin{code}
eval _ = error "incorrect prefix expression tree" 
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
    [(n,s)] | n >= 0 -> [(Num n,s)]
            | otherwise -> []
\end{code}
Let's define some operators that our parsers will recognize.
\begin{code}
[sAdd,sSub,sMul,sDiv,sMod,sNeg,sFac] = "+-*/%-!"
\end{code}
To parse an unary operator, we need to recognize one of the symbols for such operators:
\begin{code}
unaryOp :: Parser Token
unaryOp (c:s) | c == sNeg = [(Op1 negate, s)]
unaryOp (c:s) | c == sFac = [(Op1 (\n->product[1..n]), s)]
unaryOp _ = []
\end{code}
The same logic applies to binary operators:
\begin{code}
binaryOp :: Parser Token
binaryOp (c:s) | c == sAdd = [(Op2 (+), s)]
binaryOp (c:s) | c == sSub = [(Op2 (-), s)]
binaryOp (c:s) | c == sMul = [(Op2 (*), s)]
binaryOp (c:s) | c == sDiv = [(Op2 div, s)]
binaryOp (c:s) | c == sMod = [(Op2 mod, s)]
binaryOp _ = []
\end{code}
Since spaces can separate numbers from operators, we need a function to augment our parsers so that they consume spaces before recognizing tokens.
\begin{code}
spaces :: Parser a -> Parser a 
spaces p (' ':s) = spaces p s
spaces p s = p s
\end{code}
We now that expressions are formed with trees of tokens, so we need a function that will use our basic parsers and put their result into a tree.
\begin{code}
tree :: Parser a -> Parser (Tree a)
tree  = map (\(a,s) -> ([a],s)) . p

number = list (spaces num)
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
