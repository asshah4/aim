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

### AIM UTILS ### ====

#' @description Determine which project row should be used
#' @noRd
which_project_row <- function(project, which_data = NULL) {

	# If no data set is selected, default to most recent
	if (is.null(which_data)) {
		n <- length(project$title)
		#data_name <- project$title[n]
	}
	# If data set is selected, return / select that row
	else if (!is.null(which_data)) {
		# Validate it is a name within the project titles
		if (!(which_data %in% project$title)) {
			stop("Data set from `which_data` parameter is not in `project()`")
		}
		n <- match(which_data, project$title)
	}

	# Return appropriate project row name
	n

}

#' @description Ensure that the project data is available for the arm
#' @noRd
validate_project <- function(project, stage) {

	# Check for set_data()
	if (stage == "data") {
		if (!("project" %in% class(project))) {
			stop("This is not a object of class `project`")
		}
	}

	# Check for add_hypothesis()
	if (stage == "hypothesis") {
		if (!("project" %in% class(project))) {
			stop("This is not a object of class `project`")
		}
		# Ensure there is a data set available
		if (length(project$title) == 0) {
			message("There is no data available to add hypotheses against.")
		}
		# Only one data set
		else if (length(project$title) == 1) {
			message("Using the only available data set.")
		}
		# If more than one data set
		else if (length(project$title > 1)) {
			message("Defaults to the most recent data placed by `set_data()`")
		}
	}

	# Check for build_models()
	if (stage == "build") {
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
	if (stage == "findings") {
		if (!("project" %in% class(project))) {
			stop("This is not a object of class `project`")
		}
		# Check to see if fitted
		status <- unlist(project$findings, recursive = FALSE)
		if (is.null(status)) {
			stop("Cannot return findings until tests have been run with `build_models()`.")
		}
	}

}


### OCTOMOD UTILS ### ====

#' @description Create individual options for an arm
#' @noRd
inventory <- function(octomod, title, approach, pars, strata, ...) {

	# Return type of approach, whether model or test class
	type <- type_of_approach(approach)

	# "Regenerate" the lost arm if the approach is not function-like
	if (type == "htest") {
		approach <- generate_new_function(approach)
	}

	# Data splitting
	# Grouping variable is based on `core` data
	if (!is.null(strata)) {
		level <- unique(dplyr::pull(octomod$core, !!strata))
	} else {
		level <- NA
	}

	# Create status for each arm
	status <- list(
		# Test parameters
		test = list(
			type = type,
			approach = approach,
			args = pars
		),
		# Splitting parameters
		strata = list(
			split = ifelse(!is.null(strata), TRUE, FALSE),
			var = strata,
			level = level
		),
		# Fitting measures
		fit = list(
			equipped = ifelse(title %in% names(octomod$equipment), TRUE, FALSE),
			sharpened = FALSE
		)
	)

}

#' @description Check on the **which_arms** parameter
#' @noRd
bear_arms <- function(octomod, which_arms) {

	# Check which_arms argument
	if (is.null(which_arms)) {
		bear <- names(octomod$arms)
	} else {
		if (setequal(intersect(which_arms, names(octomod$core)), which_arms)) {
			stop("The named arms are not available in the `octomod`", call. = FALSE)
		} else {
			bear <- which_arms
		}
	}

	# Return character vector of arms
	bear

}

#' @description Validate the octomod structure within other functions
#' @noRd
validate_octomod <- function(octomod) {

	# Check if its octomod in pipeline
	if (!inherits(octomod, "octomod")) {
		stop("The argument must inherit from the `octomod` class.")
	}

}

#' @description Validate new arm being added to octomod
#' @noRd
validate_new_arm <- function(octomod, title, plan, exposure, pattern, approach, strata) {

	# Validate octomod first
	validate_octomod(octomod)

	# Check if core data is present
	if ("list" %in% class(octomod$core)) {
		warning("As the core has not yet been loaded, cannot check specified formula against data for available columns.")
		core_status <- FALSE
	} else{
		core_status <- TRUE
	}

	# Check if title is appropriate
	if (!is.null(title) && exists(title, octomod[["arms"]])) {
		stop("The names or `title` of an arm should be unique.")
	}

	# Check if plan is a formula
	if (!inherits(plan, "formula")) {
		stop("The listed `plan` is not a formula.")
	}

	#  Check if pattern is appropriate
	patterns <- c("direct", "sequential", "parallel")
	if (!pattern %in% patterns | length(pattern) > 1) {
		stop(paste(
			"The `pattern` should be a single string of:",
			paste(patterns, collapse = ", ")
		))
	}

	# If core is available, can double check names are appropriate
	if (FALSE) {

		# Check to see if exposure is available in core data
		if (!is.null(exposure)) {

			# Strip exposure to simplest version
			exp <- unique(unlist(strsplit(gsub("[^[:alnum:] ]", "", exposure), " +")))[-1]

			if (length(setdiff(exp, names(octomod$core))) > 0) {
				stop("Exposure variables for the data are not available in specified `core`.")
			}
		}

		# Check to see if grouping variable is allowable
		if (!is.null(strata)) {
			if (!strata %in% names(octomod$core)) {
				stop("Splitting variable for the data is not available in specified `core`.")
			}
		}

	}

}

#' @description Create a "fail-safe" execution of fit to continue running models
#' @noRd
possible_fit <- purrr::possibly(parsnip::fit.model_spec, otherwise = NA, quiet = FALSE)
