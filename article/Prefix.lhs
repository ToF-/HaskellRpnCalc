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

\begin{document}
\setlength{\parindent}{0em}
Let's write a parser for expression in prefix notation. We will need built-in functions to detect a space or a digit character
\begin{code}
module Prefix
where
import Data.Char (isSpace, isDigit, digitToInt)
\end{code}{code}

A \emph{parser} is a function that breaks a string into components called \emph{tokens}.
\begin{code}
type Parser = String -> [(Token,String)]
\end{code}
Typically, a function of type @Parser@ will examine its given String argument looking for a specific token. If the token is found, it will be returned in a list, along with the part of the string that remains to be parsed. If the token is not present, an empty list is returned. 
The token that can be parsed in a prefix expression is one of:
\begin{itemize}
\item A number,
\item An operator representing an unary function, followed by its operand,
\item A operator representing a binary function, followed by its two parameters. 
\end{itemize}
Each operand can be in turn, a full prefix expression. 
For instance, the expression @*+ 42 17 !5@ can be parsed into the @PrefExp@ value: \\
@ Op2 (*)@\\
@   (Op2 (+)@\\
@      (Num 42)@ \\
@      (Num 17))@\\
@   (Op1@ (!)\\
@      (Num 5))@\\

Since functions cannot be shown (try to enter @(+)@ at the \emph{ghci} prompt to see why), we put additional information in the definition of @PrefExp@ values so that showing them make sense.
\begin{code}
type Number = Integer
type Token = PrefExp
type Symbol = Char
data PrefExp = Num Number
             | Op1 Symbol (Number -> Number) PrefExp 
             | Op2 Symbol (Number -> Number -> Number) PrefExp PrefExp
\end{code}
To make values of the type @PrefExp@ visible (in ghci for example), we define its show function:
\begin{code}
instance Show PrefExp where
    show (Num n) = show n
    show (Op1 c _ prefExp) = (c : " ") ++ show prefExp 
    show (Op2 c _ prefExp1 prefExp2) = 
        (c :" ") ++ show prefExp1 ++ " " ++ show prefExp2
\end{code}

To parse a number 
\begin{code}
number :: Parser
number (c:s) | isSpace c = number s
number (c:s) | isDigit c = number' (digitToInt c) s
    where
    number' acc [] = [(Num (fromIntegral acc), [])]
    number' acc (c:s) | isDigit c = number' (acc * 10 + (digitToInt c)) s
                      | otherwise = [(Num (fromIntegral acc), c:s)]
number _ = []
    

\end{code}

\end{document}
