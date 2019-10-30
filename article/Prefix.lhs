\documentclass{article}

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
import Data.Char (isSpace, isDigit)
\end{code}{code}

A \emph{parser} is a function that breaks a string into components called \emph{tokens}.
\begin{code}
type Parser = String -> [(Token,String)]
\end{code}

\begin{code}
type Token = PrefExp
data PrefExp = Num Number
             | Op1 (Number -> Number) PrefExp
             | Op2 (Number -> Number -> Number) PrefExp PrefExp
\end{code}
\end{document}
