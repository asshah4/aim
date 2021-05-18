#' Initialize a Project
#'
#' This function creates the base structure for a research project. Specific
#' tests and models can be added, and subsequently built, upon a prescribed data
#' set.
#'
#' @return `project` object
#'
#' @export
project <- function() {

	# The project contains data, hypotheses, and tests
	# Each added hypothesis goes under an associated data set
	project <- tibble::tibble(
		title = character(),
		data = list(),
		hypothesis = list(),
		findings = list()
	)

	# Return after initialization
	project <- new_project(project)

}

#' @description Create an object of type `project`
#' @noRd
new_project <- function(project) {

	# Obtain classes
	classes <- class(project)

	# Define structure
	structure(project, class = c("project", classes))

}
