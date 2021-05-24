#' Build the Models and Tests
#'
#' The project arms may include hypotheses that are represented by multiple
#' types of tests. The models are dependent on the universal definitions from
#' the [{parsnip}](https://parsnip.tidymodels.org/) package. In addition,
#' specific basic statistical testing can be analyzed and stored.
#' This step is added after `add_hypothesis()` in terms of workflow. This
#' function only runs on hypothesis that have not already been run, allowing for
#' the ease of adding to an existing project without rebuilding it.
#'
#' @return Object of `project` class
#'
#' @param project Object of class `project`
#'
#' @importFrom dplyr mutate filter
#' @importFrom rlang !! sym
#' @export
build_models <- function(project, ...) {

	# Validation of project class
	if (!inherits(project, "project")) {
		stop("The argument must inherit from the `project` class.")
	}

	# Data must be available
	if (length(project$data) == 0) {
		stop("Cannot test hypotheses without data.")
	}

	# Hypotheses must be available
	if (length(project$hypothesis) == 0) {
		stop("There are no hypotheses to be tested.")
	}

	# Model building
	for (i in 1:length(project$title)) {

		# Obtain major variables
		data <- project$data[[i]]
		arm <- project$hypothesis[[i]]
		status <- project$status[[i]]
		finding <- project$hypothesis[[i]]

		# Identify number of arms
		which_arms <- names(arm)

		# Loop through each arm of the hypothesis
		for (j in 1:length(which_arms)) {

			# Determine if arm has been run already, and iterate forward
			if (status[[j]]$run == TRUE) { next }

			# If *parsnip*, both strata and not
			if (status[[j]]$type == "model_spec") {
				finding[[j]] <- arm[[j]] %>%
					dplyr::rowwise() %>%
					# Fit models using parsnip
					{
						if (status[[j]]$split == TRUE) {
							mutate(., fit = list(possible_fit(
								tests,
								formulas,
								data = filter(data, !!sym(status[[j]]$strata) == level)
							)))
						}
						else
							mutate(., fit = list(possible_fit(tests, formulas, data = data)))
					} %>%
					# Broom to tidy
					mutate(tidied = list(broom::tidy(
						fit, conf.int = TRUE, exponentiate = TRUE
					))) %>%
					ungroup()
			}

			# If *htest*, evaluate both strata and not
			if (status[[j]]$type == "htest") {
				finding[[j]] <- arm[[j]] %>%
					dplyr::rowwise() %>%
					# Create htest variables
					{
						if (status[[j]]$split == TRUE) {
							mutate(., fit = list({
								data = dplyr::filter(data, !!sym(status[[j]]$strata) == level)
								x <- data[[outcomes]]
								y <- data[[vars]]
								possible_call(tests, c(list(x, y), status[[j]]$options))
							}))
						}
						else {
							mutate(., fit = list({
								x <- data[[outcomes]]
								y <- data[[outcomes]]
								possible_call(tests, c(list(x, y), status[[j]]$options))
							}))
						}
					} %>%
					# Broom to tidy
					mutate(tidied = list(broom::tidy(
						fit, conf.int = TRUE, exponentiate = TRUE
					))) %>%
					ungroup()
			}

			# Subset finding columns
			finding[[j]] <- finding[[j]][c("number", "outcomes", "vars", "fit", "tidied")]

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

#' @description Create a "fail-safe" execution of hypothesis testing
#' @noRd
possible_call <- purrr::possibly(do.call, otherwise = NA, quiet = FALSE)
