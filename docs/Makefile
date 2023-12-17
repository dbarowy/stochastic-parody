.PHONY: all clean

all: docs.pdf

specs: specifications.tex references.bib
	pdflatex specifications.tex
	bibtex specifications.aux
	pdflatex specifications.tex
	pdflatex specifications.tex

clean:
	@rm -f *.out *.log *.bbl *.aux

