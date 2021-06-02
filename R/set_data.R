#' Set the Data
#'
#' Define the data set that should be used for analysis or tests. The data will
#' be paired with the hypotheses or tests that will be performed. If the project
#' already has a data set added, another will be added without effecting prior
#' tests.
#'
#' @return `project` object
#'
#' @param project Object of class `project`
#'
#' @param data Data frame or tibble
#'
#' @param ... For extensibility
#'
#' @export
set_data <- function(project, data, ...) {

	# Name of original dataset
	data_name <- deparse(substitute(data))

	# Add data to new row
	project <- tibble::add_row(
		project,
		title = data_name,
		data = list(data)
	)

	# Rename project data
	names(project$data) <- project$title

	# Return
	project

}

#' @description Ensure that the project data is available for the arm
#' @noRd
validate_project_data <- function(project) {

	# Ensure there is a data set available
	if (length(project$title) == 0) {
		stop("There is no data available.")
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
