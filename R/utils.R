#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' @description Ensure that the project data is available for the arm
#' @noRd
validate_project <- function(project, .stage, ...) {

	# Check for set_data()
	if (.stage == "data") {
		# Ensure project
		if (!("project" %in% class(project))) {
			stop("This is not a object of class `project`")
		}
		# Ensure that data being added is actual dataframe
		if (!inherits(..., c("data.frame", "tbl_df", "tbl"))) {
			stop("The `data` variable is not an appropriate table or data.frame class.")
		}
	}

	# Check for add_hypothesis()
	if (.stage == "hypothesis") {
		# Ensure project
		if (!("project" %in% class(project))) {
			stop("This is not a object of class `project`")
		}
		# Ensure there is a data set available
		if (length(project$data) == 0) {
			message("There is no data available to add hypotheses against.")
		}
		# Only one data set
		else if (length(project$data) == 1) {
			message("Using the only available data set.")
		}
		# If more than one data set
		else if (length(project$data) > 1) {
			message("Defaults to the most recent data placed by `set_data()`")
		}
		# Confirm formula is appropriate
		if (!("formula" %in% class(...))) {
			stop("The formula definition is not correct.")
		}
	}

	# Check for build_models()
	if (.stage == "build") {
		if (!("project" %in% class(project))) {
			stop("This is not a object of class `project`")
		}
		if (length(project$data) == 0) {
			stop("Cannot test hypotheses without data from `set_data()`.")
		}
		if (length(project$hypothesis) == 0) {
			stop("There are no hypotheses to be tested from `add_hypothesis()`.")
		}
	}

	# Check for collect_findings()
	if (.stage == "results") {
		if (!("project" %in% class(project))) {
			stop("This is not a object of class `project`")
		}
		# Check to see if fitted
		status <- unlist(project$findings, recursive = FALSE)
		if (is.null(status)) {
			stop("Cannot return findings until tests have been run with `build_models()`.")
		}
		# Ensure names are appropriate
		if (!is.null(...)) {
			if (!(... %in% names(project$hypothesis))) {
				stop("Requested hypothesis have an invalid name.")
			}
		}
	}

}

#' @description Create a "fail-safe" execution of fit to continue running models
#' @noRd
possible_parsnip_fit <- purrr::possibly(parsnip::fit.model_spec, otherwise = NA, quiet = FALSE)

#' Fit a list of {parsnip} models
#'
#' @description This allows for the simple fitting of multiple models. It
#'   requires three components: a formula, a {parsnip} model definition, and a
#'   data set to analyze.
#' @return Returns a list of model fits
#' @param .formula List or vector of formulas
#' @param .test A model definition from {parsnip}
#' @param .opts Options to pass to the test, if needed
#' @param .data Data set to be used
#' @export
#' @family studies
fit_parsnip_models <- function(.formula, .test, .opts = NULL, .data) {

	purrr::map(.formula, ~ possible_parsnip_fit(.test, .x, .data))

}

#' @description Create a "fail-safe" execution of hypothesis testing
#' @noRd
possible_call <- purrr::possibly(do.call, otherwise = NA, quiet = FALSE)

#' Fit a list of `htest` objects
#'
#' @description This allows for the simple testing of multiple h-test objects.
#'   It requires three components: a formula, a {parsnip} model definition, and
#'   a data set to analyze.
#' @return Returns a list of `htest` objects
#' @param .formula List or vector of formulas
#' @param .test An `htest` to be called
#' @param .opts Options to pass to the test, if needed
#' @param .data Data set to be used
#' @export
fit_calls <- function(.formula, .test, .opts = NULL, .data) {

	purrr::map(.formula, ~ {
		df <- model.frame(.x, .data)
		possible_call(.test, c(list(df[[1]], df[[2]]), .opts))
	})

}

#' Tidy a list of `htest` or model objects
#'
#' @description Accepts a list of objects that can be tidied via
#'   [[broom::tidy()]]
#' @return Returns a list of tidy objects in the form of tibbles of parameters
#' @param .fits List of model or `htest` objects that have been fitted
#' @inheritParams broom::tidy.lm
#' @export
tidy_tests <- function(.fits, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) {

	purrr::map(.fits, ~ broom::tidy(.x, conf.int, conf.level, exponentiate))

}


