#' Build the Models and Tests
#'
#' The project arms may include hypotheses that are represented by multiple
#' types of tests. The models are dependent on the universal definitions from
#' the [{parsnip}](https://parsnip.tidymodels.org/) package. In addition,
#' specific basic statistical testing is also able to be analyzed and stored.
#' This step is added after `add_hypothesis()` in terms of workflow.
#'
#' @return Object of `project` class
#'
#' @param project Object of class `project`
#'
#' @importFrom dplyr mutate filter
#' @importFrom rlang !! sym
#' @export
build_models <- function(project, ...) {

	# For all datasets
	for (i in 1:length(project$title)) {

		# Obtain major variables
		data <- project$data[[i]]
		arm <- project$hypothesis[[i]]
		status <- project$status[[i]]
		finding <- project$findings[[i]]

		# Identify number of arms
		which_arms <- names(arm)

		# Loop through each arm of the hypothesis
		for (j in 1:length(which_arms)) {

			# If parsnip model, both strata and not
			if (status[[j]]$type == "model") {
				finding[[j]] <- arm[[j]] %>%
					dplyr::rowwise() %>%
					{
						if (status[[j]]$split == TRUE) {
							dplyr::mutate(., fit = list(possible_fit(
								tests,
								formulas,
								data = dplyr::filter(data, !!sym(status[[j]]$strata) == level)
							)))
						}
						else
							mutate(., fit = list(possible_fit(tests, formulas, data = data)))
					}

			}

			# Subset finding columns
			finding[[j]] <- finding[[j]][c("test_num", "outcomes", "vars", "fit")]

			# Set status
			status[[j]]$run <- TRUE
		}

		# Place data back
		names(finding) <- which_arms
		project$findings[[i]] <- finding
		project$status[[i]] <- status

	}

	# Return
	project
}

#' @description Create a "fail-safe" execution of fit to continue running models
#' @noRd
possible_fit <- purrr::possibly(parsnip::fit.model_spec, otherwise = NA, quiet = FALSE)
