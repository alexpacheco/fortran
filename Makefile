TEX = pdflatex 

.DEFAULT = all
.PHONY = all view

all : $(FILE).pdf

view : $(FILE).pdf
	open $(FILE).pdf

$(FILE).pdf : $(FILE).tex 
	$(TEX) $(FILE).tex  
	$(TEX) $(FILE).tex  
clean:
	rm -f $(FILE).aux $(FILE).log $(FILE).blg $(FILE).bbl $(FILE).out $(FILE).pdf

