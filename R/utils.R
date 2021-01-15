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
validate_new_arm <- function(octomod, title, plan, exposure, pattern, approach, split) {

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
	if (core_status) {

		# Check to see if exposure is available in core data
		if (!is.null(exposure)) {
			if (length(setdiff(exposure, names(octomod$core))) > 0) {
				stop("Exposure variables for the data are not available in specified `core`.")
			}
		}

		# Check to see if grouping variable is allowable
		if (!is.null(split)) {
			if (!split %in% names(octomod$core)) {
				stop("Splitting variable for the data is not available in specified `core`.")
			}
		}

	}

}
