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
	rm -f $(FILE).aux $(FILE).log $(FILE).nav $(FILE).out 
	rm -f $(FILE).pdf $(FILE).snm $(FILE).toc $(FILE).vrb
