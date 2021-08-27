#' Validate `hypothesis` object
#' @noRd
validate_hypothesis <- function(h) {

	# Class
	validate_class(h, "hypothesis")

	# Data
	validate_class(attr(h, "data"), c("tbl", "tbl_df", "data.frame"))

	# Combination
	validate_class(attr(h, "combination"), "character")

	# Check optional parameters

	# Return check
	invisible(TRUE)

}

#' Validation of class inheritance
#' @noRd
validate_class <- function(x, what) {
	if (!inherits(x, what)) {
		stop(
			deparse(substitute(x)),
			" needs to inherit from `",
			paste("c(", paste(what, collapse = ", ") , ")", sep = ""),
			"`, but is of class `",
			class(x),
			"`.",
			call. = FALSE
		)
	}

	# Return
	invisible(TRUE)
}

#' Validate that study is at the appropriate stage for subsequent pipe functions
#' @noRd
validate_stage <- function(x, stage) {

	# Get status table
	y <- attr(x, "status_table")

	# Validate that the study contains hypothesis objects
	switch(
		stage,
		hypothesis = {
			if (nrow(y) == 0) {
				stop(
					deparse(substitute(x)),
					" does not yet have `hypothesis` objects added. Please call `draw_hypothesis()` prior to construction.",
					call. = FALSE
				)
			}
		},
		run = {
			if (!any(y$run)) {
				stop(
					deparse(substitute(x)),
					" does not have any `hypothesis` objects that have been run. Please call `construct_models()` first.",
					call. = FALSE
				)
			}
		}
	)

	# Return
	invisible(TRUE)
}
