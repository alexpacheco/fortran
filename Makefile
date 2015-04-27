TEX = pdflatex 
JOB = $(FILE)

.DEFAULT = all
.PHONY = all view

all : $(JOB).pdf

view : $(JOB).pdf
	open $(JOB).pdf

$(JOB).pdf : $(JOB).tex 
	$(TEX) $(JOB).tex  
	$(TEX) $(JOB).tex  
clean:
	rm -f $(JOB).aux $(JOB).log $(JOB).blg $(JOB).bbl $(JOB).out $(JOB).pdf

