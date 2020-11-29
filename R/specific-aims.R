#' @title Specific Aims
#'
#' @description Every research project, particularly proposals, starts with a specific aims page, which contains the aims and sub-aims intended to be studied along with the corresponding hypothesis. The `specific_aims()` function is a method to collect all the related `aim` objects from a project to help track, update, and manage the research project.
#'
#' @param ... Variable number of `aim` objects. The name of the objects will be retained.
#'
#' @return An `aims` object that is a collection of the related individual `aim` objects from a project.
#'
#' @export
specific_aims <- function(...) {

	# Setup names of aims
	names_of_aims <- as.character(substitute(list(...)))[-1L]
	aims <- list(...)
	names(aims) <- names_of_aims

	# Return
	aims <- new_aims(aims)
	return(aims)

}

#' @description Construct a specific aims object
#' @noRd
new_aims <- function(aims) {

	# Confirm that the "aims" is a table in an aims format
	stopifnot(is.list(aims))

	# Check to see if already has aim class attached
	if ("aims" %in% class(aims)) {
		structure(aims, class = class(aims))
	} else {
		structure(aims, class = c("aims", class(aims)))
	}

}
