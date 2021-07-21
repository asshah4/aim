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
	# Each added hypothesis has a unique name
	# Instructions tell how hte hypothesis is applied to the data
	# Findings contain final outcomes/results
	project <- list(
		data = list(),
		hypothesis = list(),
		findings = list(),
		instructions = list()
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

#' @description Generic print method
#' @param x Object of class `project`
#' @param ... arguments to pass on
#' @noRd
#' @export
print.project <- function(x, ...) {

	# Construction of basic components
	hypothesis <-
		x$hypothesis %>%
		dplyr::bind_rows(.id = "name")

	instructions <-
		x$instructions %>%
		dplyr::bind_rows(.id = "name")

	findings <-
		x$findings %>%
		dplyr::bind_rows(.id = "name")

	# Make combination of hypothesis and instructions for summary
	if (length(hypothesis) > 0) {
		project <-
			dplyr::inner_join(hypothesis, instructions, by = "name") %>%
			dplyr::select(
				dplyr::one_of(
					"data",
					"name",
					"type",
					"outcomes",
					"number",
					"data",
					"run",
					"split",
					"level"
				)
			) %>%
			suppressWarnings()

		# Clean up levels/strata/splits
		if ("level" %in% names(project)) {
			project <-
				project %>%
				dplyr::group_by(name, level) %>%
				dplyr::filter(number == max(number)) %>%
				dplyr::ungroup()
		} else {
			project <-
				project %>%
				dplyr::select(-split) %>%
				dplyr::group_by(name) %>%
				dplyr::filter(number == max(number)) %>%
				dplyr::ungroup()
		}
	}

	# If there is only data and no hyptheses
	if (length(hypothesis) == 0) {
		project <-
			tibble::tribble(
				~data, ~hypothesis, ~findings,
				names(x$data), length(hypothesis), length(findings)
			) %>%
			tidyr::unnest(data)
	}

	# Return
	print(project)
}
