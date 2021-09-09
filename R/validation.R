# nocov start

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

#' Check if hypothesis in study has been run or not
#' @noRd
check_hypothesis <- function(x, name, run = TRUE) {

	# Get status table
	y <- attr(x, "status_table")
	if (run %in% y$run[y$name == name]) {
		# Hypothesis has requested status
		invisible(TRUE)
	} else {
		stop("Hypothesis `",
				 name,
				 "` should have run status of `",
				 run,
				 "` for this test."
		)
	}

}

#' Check if formula terms are in data set
#' @noRd
check_terms <- function(x, data) {

	# Strip away unnecessary elements from formula to get pure terms
	vars <- all.vars(x)
	cols <- names(data)

	if (!all(vars %in% cols)) {
		y <- vars[which(!(vars %in% cols))]
		stop(
			"The terms c(",
			paste0("'", y, "'", collapse = ", "),
			") are not contained in the data set `",
			deparse(substitute(data)),
			"`."
		)
	}


}

# nocov end
