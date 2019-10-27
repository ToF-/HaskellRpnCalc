\documentclass{article}

%include lhs2TeX.fmt
%include lhs2TeX.sty
%include spacing.fmt

\begin{document}
\setlength{\parindent}{0em}
Let's import the @Hspec@ library, and the module which will contain the code for our calculator:
\\
\begin{code}
import Test.Hspec
import Rpn
\end{code}\\
Then we are ready to launch some checks.

\begin{code}
main = hspec $ do
    describe "rpn" $ do
        it "passes some tests" $ do
             2+2  `shouldBe` 4
\end{code}
\end{document}



