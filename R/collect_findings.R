#' Collect the Important Findings from a Project
#'
#' This function simplifies the collection of the model fit and test results
#' from a function. This should be called after the project has been fitted with
#' `build_models()`.
#'
#' @return Returns a `tibble` that includes the name of the hypothesis, the test
#'   number, the outcome variable, the terms, and model or test parameters as
#'   defined by the [broom::tidy()] function. If requested, can instead return
#'   the original model fit instead of tidy parameters.
#'
#' @param project Object of class `project`
#'
#' @param which_tests Character vector of hypothesis names to be collected.
#'   Defaults to all tests that were run.
#'
#' @param tidy Returns findings after being processed by [broom::tidy()],
#'   defaulting to TRUE. If the original model fit is required, change to FALSE.
#'
#' @param .stage Internal marker of workflow progress.
#'
#' @param ... For extensibility
#'
#' @export
collect_findings <- function(project, which_tests = NULL, tidy = TRUE, .stage = "results", ...) {

	# Validate project
	validate_project(project, .stage, which_tests)

	# Subset tests if needed
	if (is.null(which_tests)) {
		findings <- project$findings
	} else {
		which_tests <- match(which_tests, names(project$findings))
		findings <- project$findings[[which_tests]]
	}

	# Either RAW or TIDY findings
	if (!tidy) {
		message("Only returning raw models.")
		# Ensure only findings that are from model specs
		findings <-
			project$instructions %>%
			dplyr::bind_rows(.id = "name") %>%
			dplyr::filter(type == "model_spec") %>%
			dplyr::pull(name) %>%
			intersect(., names(findings)) %>%
			findings[.] %>%
			dplyr::bind_rows(.id = "name") %>%
			dplyr::select(-c(vars, tidied)) %>%
			dplyr::rowwise() %>%
			# Select the actual models
			dplyr::mutate(fit = list(fit$fit)) %>%
			dplyr::ungroup()
	} else {
		# Tidy findings
		findings <-
			project$findings %>%
			dplyr::bind_rows(.id = "name") %>%
			tidyr::unnest(tidied) %>%
			dplyr::select(-c(vars, fit))
	}

	# Return
	findings

}
