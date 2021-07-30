#' Validation of class inheritance
#' @noRd
validate_class <- function(x, what) {
	if (!inherits(x, what)) {
		stop(
			deparse(substitute(x)), " needs to inherit from `", what, "`, but is of class `", class(x), "`.",
			call. = FALSE
		)
	}

	# Return
	invisible(TRUE)
}
