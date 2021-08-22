#' Draw A Hypothesis
#' @rdname hypothesize
#' @export
hypothesize <- function(h, ...) {
	UseMethod("hypothesize", object = h)
}

#' @rdname hypothesize
#' @export
hypothesize.formula <- function(h,
																combination,
																test,
																test_opts = NULL,
																data,
																strata = NULL) {

	# Check for test_opts
	if (is.null(test_opts)) {test_opts <- NA}

	# Check for strata
	if (is.null(strata)) {strata <- NA}

	# Construct
	h <- new_hypothesis(
		hypothesis = h,
		combination = combination,
		test = test,
		test_opts = test_opts,
		data = data,
		strata = strata
	)

	# Validate
	validate_hypothesis(h)

	# Return
	h

}

#' @rdname hypothesize
#' @export
hypothesize.default <- function(h, ...) {
	stop(
		"`hypothesize()` is not defined for a `", class(h)[1], "` object.",
		call. = FALSE
	)
}

#' Constructor for `hypothesis` class
#' @noRd
new_hypothesis <- function(hypothesis,
													 combination,
													 test,
													 test_opts,
													 data,
													 strata) {

	structure(
		hypothesis,
		combination = combination, # Formula building pattern
		test = test, # Either model_spec or htest object
		test_opts = test_opts, # Additional arguments to pass down
		data = data, # List of data frames, named
		strata = strata, # List of variables contained in linked data frames
		class = c("hypothesis", class(hypothesis)) # Class definition
	)

}
