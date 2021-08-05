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

#' @description Determine which project row should be used
#' @noRd
which_project_row <- function(project, which_data = NULL) {
	# If no data set is selected, default to most recent
	if (is.null(which_data)) {
		n <- length(project$title)
	}

	# If data set is selected, return / select that row
	if (!is.null(which_data)) {
		# Validate it is a name within the project titles
		if (!(which_data %in% names(project$title))) {
			stop("Data set from `which_data` parameter is not in `project()`")
		}
		else {
			n <- match(which_data, names(project$title))
		}
	}

	# Return appropriate project row name
	n

}

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
#' @family frameworks
fit_models <- function(.formula, .test, .opts = NULL, .data) {

	purrr::map(.formula, ~ possible_fit(.test, .x, .data))

}
