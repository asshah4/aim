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
#' @param .stage Internal marker of workflow progress.
#'
#' @param ... For extensibility
#'
#' @importFrom dplyr mutate filter select
#' @importFrom rlang !! sym
#' @export
build_models <- function(project, .stage = "build", ...) {

	# Validate project
	validate_project(project, .stage)

	# Model building
	for (i in 1:length(project$hypothesis)) {

		# Obtain major variables
		name <- names(project$hypothesis)[i]
		instructions <- project$instructions[[name]]
		data_name <- instructions$data
		data <- project$data[[data_name]]
		arm <- project$hypothesis[[name]]

		# Determine if arm has been run already, and iterate forward
		if (instructions$run == TRUE) { next }

		# If *parsnip*, both strata and not
		if (instructions$type == "model_spec") {
			finding <- arm %>%
				dplyr::rowwise() %>%
				# Fit models using parsnip
				{
					if (instructions$split == TRUE) {
						mutate(., fit = list(possible_fit(
							tests,
							formulas,
							data = filter(data, !!sym(instructions$strata) == level)
						)))
					}
					else
						mutate(., fit = list(possible_fit(tests, formulas, data = data)))
				} %>%
				# Broom to tidy
				mutate(tidied = list(broom::tidy(
					fit, conf.int = TRUE, exponentiate = TRUE
				))) %>%
				dplyr::ungroup()
		}

		# If *htest*, evaluate both strata and not
		if (instructions$type == "htest") {
			finding <- arm %>%
				dplyr::rowwise() %>%
				# Create htest variables
				{
					if (instructions$split == TRUE) {
						mutate(., fit = list({
							data = dplyr::filter(data, !!sym(instructions$strata) == level)
							x <- data[[outcomes]]
							y <- data[[vars]]
							possible_call(tests, c(list(x, y), instructions$options))
						}))
					}
					else {
						mutate(., fit = list({
							x <- data[[outcomes]]
							y <- data[[vars]]
							possible_call(tests, c(list(x, y), instructions$options))
						}))
					}
				} %>%
				# Broom to tidy
				mutate(tidied = list(broom::tidy(
					fit, conf.int = TRUE, exponentiate = TRUE
				))) %>%
				dplyr::ungroup()
		}

		# Subset finding columns
		finding <- finding %>%
			select(dplyr::one_of("number", "level", "outcomes", "exposures", "vars", "fit", "tidied")) %>%
			suppressWarnings()

		# Set status
		instructions$run <- TRUE

		# Replace variables back into project prior to next loop
		project$findings[[name]] <- finding
		project$instructions[[name]] <- instructions
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

#' Fit a list of {parsnip} models
#'
#' @description This allows for the simple fitting of multiple models. It
#'   requires three components: a formula, a {parsnip} model definition, and a
#'   data set to analyze.
#' @return Returns a list of model fits
#' @param .formula List or vector of formulas
#' @param .test A model definition from {parsnip}
#' @param .opts Options to pass to the test, if needed
#' @param .data Data set to be used
#' @export
#' @family frameworks
fit_models <- function(.formula, .test, .opts = NULL, .data) {

	purrr::map(.formula, ~ possible_fit(.test, .x, .data))

}

#' Fit a list of `htest` objects
#'
#' @description This allows for the simple testing of multiple h-test objects.
#'   It requires three components: a formula, a {parsnip} model definition, and
#'   a data set to analyze.
#' @return Returns a list of `htest` objects
#' @param .formula List or vector of formulas
#' @param .test An `htest` to be called
#' @param .opts Options to pass to the test, if needed
#' @param .data Data set to be used
#' @export
fit_calls <- function(.formula, .test, .opts = NULL, .data) {

	purrr::map(.formula, ~ {
		df <- model.frame(.x, .data)
		possible_call(.test, c(list(df[[1]], df[[2]]), .opts))
	})

}

#' Tidy a list of `htest` or model objects
#'
#' @description Accepts a list of objects that can be tidied via
#'   [[broom::tidy()]]
#' @return Returns a list of tidy objects in the form of tibbles of parameters
#' @param .fits List of model or `htest` objects that have been fitted
#' @inheritParams broom::tidy.lm
#' @export
tidy_tests <- function(.fits, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) {

	purrr::map(.fits, ~ broom::tidy(.x, conf.int, conf.level, exponentiate))

}
