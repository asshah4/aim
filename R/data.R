#' Identify number of missing
#' @export
number_of_missing <- function(x) {
	if (is.vector(x)) {
		sum(is.na(x))
	}
}

#' Identify if variable is binary or dichotomous (or not)
#' @export
is_dichotomous <- function(x) {

	n <- length(stats::na.omit(levels(factor(x))))
	if (n == 2) {
		TRUE
	} else {
		FALSE
	}

}
