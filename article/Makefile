article: Specs.lhs Rpn.lhs Calc.lhs
	lhs2tex Rpn.lhs >Rpn.tex
	pdflatex Rpn.tex
	lhs2tex Specs.lhs >Specs.tex
	pdflatex Specs.tex

test: Specs.lhs Rpn.lhs
	runhaskell Specs.lhs

main: Rpn.lhs Calc.lhs
	runhaskell Calc.lhs

clean: 
	rm *.log
	rm *.aux
	rm *.ptb
	rm *.pdf
