#' Add a Hypothesis to the Project
#'
#' Adding an "arm" to the project reflects a series of hypothesis generated
#' around the core data that was set in the preceding function. The hypothesis
#' formula allows for modifiers to both the formula itself, as well as *how* it
#' should be applied to the data set.
#'
#' @return `project` object
#'
#' @param project Object of class `project`
#'
#' @param name Character string to identify this arm. Must be unique and not
#'   previously specified.
#'
#' @inheritParams make_formulas
#'
#' @param test The testing that will be used, defined as how the analysis or
#'   hypothesis testing should be performed. There are several options on which
#'   specification to use, anywhere from regression modeling to inferential
#'   statistics. The test applies to the collection of hypotheses.
#'
#'   * A model specified by the [`parsnip`
#'   package](https://parsnip.tidymodels.org), which includes the mode and #'   computational engine
#'
#'   * A statistical test, such as a `t.test`, which may require additional
#'   parameters. These can be given as additional, unmatched arguments. This
#'   option currently supports only hypothesis tests, of class `htest`.
#'
#' @param which_data Name of "row" or dataset that should be analyzed. Defaults
#'   to the most recent dataset added.
#'
#' @param .strata How the data should be split or stratified. References the
#'   name of the data given in `set_data()` that the models will be with fit
#'   against, splitting the data into subsets. This helps to perform hypothesis
#'   testing on subsets or strata of the data. It defaults to NULL (which means
#'   the full data will be used) **experimental**
#'
#' @param .stage Internal marker of project status.
#'
#' @param ... This should reflect the additional parameters that may need to be
#'   given to the `test` argument, such as `paired = TRUE` for `t.test()`. The
#'   additional parameters must be named to allow them to be passed
#'   successfully.
#'
#' @export
make_hypothesis <- function(project, name, formula, combination = "direct", test, which_data = NULL, .strata = NA, .stage = "hypothesis", ...) {

	# Argument validation
	validate_project(project, .stage, formula)

	# Save additional, optional parameters
	dots <- rlang::dots_list(...)
	if (length(dots) == 0) {
		test_opts <- NA
	} else {
		test_opts <- dots
	}

	# Create instructions
	instructions <- list()

	# Tie hypothesis to data
	if (!is.null(which_data)) {
		instructions$data <- which_data
	} else {
		instructions$data <- tail(names(project$data), n = 1)
	}

	# Update instructions/status
	instructions$run <- FALSE
	instructions$split <- !is.na(.strata)
	instructions$strata <- .strata
	instructions$type <- type_of_test(test)
	instructions$options <- test_opts

	project$instructions[[name]] <- dplyr::as_tibble(instructions)

	# Formula table with tests
	tbl <- make_formulas(formula, combination)
	tbl$tests <- list(test)

	# Add strata if available
	if (!is.na(.strata)) {
		tbl <-
			tidyr::expand_grid(
				tbl,
				level =
					dplyr::pull(project$data[[instructions$data]], .strata) %>%
					unique() %>%
					as.factor()
			)
	}

	# Place back into project
	project$hypothesis[[name]] <- tbl

	# Return
	project

}

#' Make formula combinations in `new_hypothesis()`
#'
#' This is an internal function that helps to create the appropriate formula
#' combinations for adding hypotheses.
#'
#' @param formula Formula showing relationship of outcomes and predictors, and
#'   is essentially the hypothesis. It allows for complex formulas e.g. multiple
#'   predictors, multiple exposures, and multiple covariates, that can be
#'   organized into individual formulas based on additional modifiers. Each RHS
#'   term of the formula is considered an outcome variable, and is analyzed as a
#'   single outcome. Each LHS term is consider a predictor, and can be modified
#'   as below:
#'
#'   * `exp()` is placed around a term to define as an independent exposure,
#'   which will be placed in separate formulas from any other term marked as an
#'   exposure
#'
#'   * `cov()` is placed around a term for any predictors that should be
#'   maintained/fixed in all models, which can include complex terms, such as
#'   mixed effects
#'
#'   Of note, these formula modifiers do not apply to `htest` objects yet.
#'
#'   For example, the equation below describes two independent exposures "x1"
#'   and "x2" that should be conditionally regressed for every level of "z"
#'
#'   \deqn{y ~ exp(x1) + exp(x2) + x3 + x4 + cov((1 | z))}
#'
#' @param combination The building pattern for how to put together the overall
#'   plan. It defines variable relationships that will be used. The options for
#'   the `combination` currently include:
#'
#'   * `direct` will define the relationship as y ~ x
#'
#'   * `sequential` will define the relationship as y ~ x1, y ~ x1 + x2
#'
#'   * `parallel` will define the relationship as y ~ x1, y ~ x2
#'
#' @importFrom dplyr mutate
#' @export
make_formulas <- function(formula, combination) {

	# Break apart formula (deparsing to help with survival/mixed objects)
	outcomes <-
		gsub(" ", "", unlist(strsplit(deparse(formula[[2]]), "\ \\+\ ")))
	predictors <- labels(stats::terms(formula))
	exposures <- grep("exp\\(", predictors, value = TRUE)
	fixed <- grep("cov\\(", predictors, value = TRUE)
	covariates <- setdiff(labels(stats::terms(formula)), c(fixed, exposures))

	# Clean up exposures
	if (length(exposures) > 0) {
		exposures <-
			gsub("\\)", "", gsub("exp\\(", "", exposures)) %>%
			paste(outcomes, ., sep = " ~ ") %>%
			lapply(., stats::formula) %>%
			lapply(., stats::terms) %>%
			lapply(., labels)
	}

	# Clean up fixed variables
	# Add parenthesis back to mixed variables
	fixed <- gsub("\\)$", "", gsub("cov\\(", "", fixed))
	fixed[grepl("\\|", fixed)] <-
		gsub("\\(", "", gsub("\\)", "", grep("\\|", fixed, value = TRUE)))
	fixed[grepl("\\|", fixed)] <-
		paste0("(", fixed[grepl("\\|", fixed)], ")")

	# Null out unneed variables
	if (length(fixed) == 0) {fixed <- NULL}
	if (length(exposures) == 0) {exposures <- NULL}

	# Place the fixed variables in the front
	predictors <- c(fixed, covariates)

	# Based on approach
	switch(
		combination,
		direct = {
			num <- sum(any(!is.null(covariates), !is.null(fixed)))

			tbl <-
				tibble::tibble(number = 1:num) %>%
				mutate(., vars = list(predictors)) %>%
				{
					if (length(exposures) > 0) {
						tidyr::expand_grid(exposures = exposures, .) %>%
						mutate(vars = purrr::map2(vars, exposures, ~ c(.y, .x))) %>%
						mutate(vars = purrr::map(vars, ~ na.omit(.x)))
					} else {
						tibble::add_column(., exposures = NA)
					}
				} %>%
				tidyr::expand_grid(outcomes = outcomes, .)
		},
		sequential = {
			# Number of covariates to sequence through
			num <- length(covariates) + sum(!is.null(exposures))

			tbl <-
				tibble::tibble(number = 1:num) %>%
				mutate(vars = purrr::map(
					number, ~ c(fixed, covariates[0:(.x - sum(!is.null(exposures)))])
				)) %>%
				{
					if (length(exposures) > 0) {
						tidyr::expand_grid(exposures = exposures, .) %>%
						mutate(vars = purrr::map2(vars, exposures, ~ c(.y, .x))) %>%
						mutate(vars = purrr::map(vars, ~ na.omit(.x)))
					} else {
						tibble::add_column(., exposures = NA)
					}
				} %>%
				tidyr::expand_grid(outcomes = outcomes, .)
		},
		parallel = {
			# If no exp or fixed, than based on num. of covariates
			# If no covariates, than based on num. of fixed and exposures
			if (length(covariates) == 0) {
				num <- sum(any(c(!is.null(fixed), !is.null(exposures))))
			}

			if (length(covariates) > 0) {
				num <- length(covariates)
			}

			tbl <-
				tibble::tibble(number = 1:num) %>%
				mutate(
					vars = purrr::map(number, ~ c(fixed, covariates[.x]))
				) %>%
				{
					if (length(exposures) > 0) {
						tidyr::expand_grid(exposures = exposures, .) %>%
						mutate(vars = purrr::map2(vars, exposures, ~ c(.y, .x))) %>%
						mutate(vars = purrr::map(vars, ~ na.omit(.x)))
					} else {
						tibble::add_column(., exposures = NA)
					}
				} %>%
				tidyr::expand_grid(outcomes = outcomes, .)
		}
	)

	# Expand formulas
	tbl <-
		tbl %>%
		mutate(formulas = purrr::map_chr(vars, ~paste(unlist(.x), collapse = " + "))) %>%
		mutate(formulas = paste(outcomes, formulas, sep = " ~ ")) %>%
		mutate(formulas = purrr::map(formulas, ~stats::formula(.x)))

	# Return
	tbl

}

#' @description Identify type of test that will be used
#' @noRd
type_of_test <- function(test) {

	# Check if modeling type
	if ("model_spec" %in% class(test)) {
		type <- "model_spec"
	}

	# Check if hypothesis test type
	if ("character" %in% class(test)) {
		# Check to see if viable function
		fn <- get(test)
		if (is.function(fn)) {
			type <- "htest"
		}
		else {
			stop("The `test` is not a function that can be passed on.")
		}
	}
	# Validation of type
	else if ("character" %in% class(test)) {
		stop("The `test` is not a character string.", call. = FALSE)
	}

	# Return
	type
}

