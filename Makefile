all:
	R -e "roxygen2::roxygenize()"
	R CMD INSTALL .
