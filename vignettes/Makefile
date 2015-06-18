all: References.pdf hsdar-intro.pdf
References.pdf: References.tex
	"$(R_HOME)/bin/R" CMD texi2dvi --pdf --clean References.tex
hsdar-intro.pdf: hsdar-intro.tex
	"$(R_HOME)/bin/R" CMD texi2dvi --pdf --clean hsdar-intro.tex
clean:
	rm hsdar-intro-* hsdar-intro.R Rplots.pdf example_in.tif example_result.tif

