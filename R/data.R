#' Identify number of missing
#' @export
number_of_missing <- function(x) {
	if (is.vector(x)) {
		sum(is.na(x))
	}
}
