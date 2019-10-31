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
Let's write a parser for expression in prefix notation. We will need built-in functions to detect a space or a digit character. 
\begin{code}
module Prefix
where
import Data.Char (isSpace, isDigit, digitToInt)
\end{code}
First let's define some operators that our parser will recognize.
\begin{code}
sPlus = '+'
sMinus = '-'
sMult  = '*'
sDiv   = '/'
sMod   = '%'
sNegate= '~'
sFact  = '!'
\end{code}
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

Our first parser should recognize a digit, convert that value from @Int@ and return the @Num@ token.
\begin{code}
digit :: Parser
digit (c:s) | isDigit c = [(digitToNum c, s)]
    where digitToNum = Num . fromIntegral . digitToInt
digit _ = []

\end{code}
Another parser converts all successive digits into a @Num@ value.\\
\begin{tabular}{rl}
$0$ & 4807 \\
$0\times{10}+4$               & 807 \\
$(0\times{10}+4)\times{10}+8$ & 07 \\
$((0\times{10}+4)\times{10}+8)$ & 07 \\
$(((0\times{10}+4)\times{10}+8)\times{10}+0)$ & 7 \\
$((((0\times{10}+4)\times{10}+8)\times{10}+0)\times{10}+7)$ &  \\
\end{tabular}

\begin{code}
accum :: Integer -> Parser
accum acc s = case digit s of
    [] -> [(Num acc, s)]
    [(Num d,s')] -> accum (acc * 10 + d) s'
\end{code}
To parse a number, ignore spaces, then if one digit is found, accumulate all the following digits into a number. Otherwise if no digit was found, yield the empty result.
\begin{code}
number :: Parser
number (c:s) | isSpace c = number s
number (c:s) | isDigit c = accum 0 (c:s)
number _ = []
\end{code}
A parser for negation should recognize the symbol @`~`@ and yield an unary operator token with the matching function. The same should be done for the symbol @'!'@ and the factorial operation. This can be generalized into a parser for any unary operator.
\begin{code}
unaryOp :: Symbol -> (Number -> Number) -> Parser
unaryOp op f (c:s) | c == op = [(Op1 op f, s)]
unaryOp _ _ _ = []

negation :: Parser
negation = unaryOp '~' negate

factorial :: Parser
factorial = unaryOp '!' (\n -> product [1..n])
\end{code}
We can combine two parsers in a way such that one or the other token can be recognized.
\begin{code}
infix 2 <|>

(<|>) :: Parser -> Parser -> Parser
parserA <|> parserB = \s -> let result = parserA s in case result of
    [] -> parserB s
    _  -> result
    

\end{code}

\end{document}
