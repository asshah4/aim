#' Collect the Important Findings from a Project
#'
#' This function simplifies the collection of the model fit and test results
#' from a function. This should be called after the project has been fitted with
#' `build_models()`.
#'
#' @return Returns a `tibble` that includes the name of the hypothesis, the test number, the outcome variable, the terms, and model or test parameters as defined by the [broom::tidy()] function.
#'
#' @param project Object of class `project`
#'
#' @param stage Internal marker of workflow progress.
#'
#' @param ... For extensibility
#'
#' @export
collect_findings <- function(project, stage = "findings", ...) {

	# Validate project
	validate_project(project, stage)

	# Create summary table of findings
	findings <-
		project$findings %>%
		# Collapse all findings into table
		unlist(recursive = FALSE, use.names = TRUE) %>%
		dplyr::bind_rows(.id = "name") %>%
		# Select out tidied values
		tidyr::unnest(tidied) %>%
		dplyr::select(-c(vars, fit))

	# Return
	findings

}
