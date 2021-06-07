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
#' @param stage Internal marker of workflow progress.
#'
#' @param ... For extensibility
#'
#' @export
set_data <- function(project, data, stage = "data", ...) {

	# Validate project
	validate_project(project, stage)

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

