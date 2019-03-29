
sources := 		$(wildcard *.Rmd)


all:			${sources}
			Rscript -e "rmarkdown::render(\"$<\", clean=TRUE)"


