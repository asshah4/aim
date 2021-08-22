#' Validate `hypothesis` object
#' @noRd
validate_hypothesis <- function(h) {

	# Class
	validate_class(h, "hypothesis")

	# Data
	validate_class(attr(h, "data"), c("tbl", "tbl_df", "data.frame"))

	# Combination
	validate_class(attr(h, "combination"), "character")

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

#' Validate that framework contains hypothesis that is being modified
#' @noRd
validate_hypothesis_exists <- function(x, name) {
	if (!(name %in% unique(x$name))) {
		stop(
			"The `hypothesis` ",
			name,
			"is not already referenced in the `framework` ",
			deparse(substitute(x)),
			"."
		)
	}

	invisible(TRUE)
}

#' Validate that data is present
#' @noRd
validate_hypothesis_data_exists <- function(x) {

	if (length(attributes(x)$data) == 0) {
		warning("No data present in `hypothesis`", deparse(substitute(x)), ".")
	}
}
