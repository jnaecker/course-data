all: data/clean-data.rda output/course-analysis.html

data/clean-data.rda: src/data-cleaning.R
	Rscript src/data-cleaning.R

output/course-analysis.html: src/course-analysis.Rmd
	Rscript -e 'library(rmarkdown); rmarkdown::render("src/course-analysis.Rmd", "html_document")'
	mv src/course-analysis.html output/course-analysis.html

clean: 
	rm data/clean-data.rda
	rm output/*