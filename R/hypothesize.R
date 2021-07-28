#' Create a Hypothesis
#'
#' `hypothesize()` creates a `hypothesis` object that can be placed into a
#' modeling framework. This framework allows for multiple hypotheses to be
#' modeled, and allows for assessment of confounding variables.
#'
#' @return An object of class `hypothesis`
#'
#' @param x General object that can be cast into a hypothesis object. Currently
#'   this accepts the following object types:
#'
#'   * `formula` objects, which will be given additional modifiers as described.
#'   Please see the `formula` parameter for further details.
#'
#' @param ... For additional variables based on the generic method invoked.
#'
#' @inheritParams expand_formula
#'
#' @param test The testing that will be used, defined as how the analysis or
#'   hypothesis testing should be performed. There are several options on which
#'   specification to use, anywhere from regression modeling to inferential
#'   statistics. The test applies to the collection of hypotheses.
#'
#'   * A model specified by the [`parsnip`
#'   package](https://parsnip.tidymodels.org), which includes the mode and
#'   computational engine
#'
#'   * A statistical test, such as a `t.test`, which may require additional
#'   parameters. These can be given as additional, unmatched arguments. This
#'   option currently supports only hypothesis tests, of class `htest`.
#'
#' @param test_opts For optional or additional parameters to be given to the
#'   *test* argument. Should be in format of a list of **name = argument**
#'   pairs.
#'
#' @inheritParams add_data
#'
#' @rdname hypothesize
#' @export
hypothesize <- function(x, ...) {
	UseMethod("hypothesize", object = x)
}

#' For general formula `hypothesis` objects
#' @rdname hypothesize
#' @export
hypothesize.formula <- function(formula,
																combination,
																test,
																test_opts = NULL,
																data = NULL,
																data_name = NULL,
																strata = NULL,
																...) {
	# Base class is a formula
	hypothesis <- stats::formula(formula)

	# The original formula and modifiers should be recorded
	attr(hypothesis, "combination") <- combination
	attr(hypothesis, "test") <- test

	# Expanded formulas are needed for evaluation
	tbl <- expand_formula(formula, combination)
	attr(hypothesis, "formulas") <- list(tbl$formulas)
	attr(hypothesis, "terms") <- tbl[1:4]

	# Data structures are optional, as may already exist in the framework
	attr(hypothesis, "data") <- list()
	attr(hypothesis, "strata") <- list()
	if (is.null(data) & is.null(data_name)) {
		warning("No `data` variable or reference was included. This hypothesis cannot be tested until its associated with a `data` object.")
		attributes(hypothesis)$data[["missing_data"]] <- NA
	} else if (!is.null(data) & is.null(data_name)) {
		data_name <- deparse(substitute(data))
		attributes(hypothesis)$data[[data_name]] <- data
	} else if (!is.null(data) & !is.null(data_name)) {
		attributes(hypothesis)$data[[data_name]] <- data
	} else if (is.null(data) & !is.null(data_name)) {
		attributes(hypothesis)$data[[data_name]] <- NA
	}

	# Return
	hypothesis <-
		structure(hypothesis, class = c("hypothesis", class(hypothesis)))
}

#' Expand Formulas for a Hypothesis
#'
#' This is an internal function that helps to create the appropriate formula
#' combinations for adding hypotheses.
#'
#' @return Table of formulas and extracted terms
#'
#' @param formula Formula showing relationship of outcomes and predictors, and
#'   is essentially the hypothesis. It allows for complex formulas e.g. multiple
#'   predictors, multiple exposures, and multiple covariates, that can be
#'   organized into individual formulas based on additional modifiers. Each RHS
#'   term of the formula is considered an outcome variable, and is analyzed as a
#'   single outcome. Each LHS term is consider a predictor, and can be modified
#'   as below:
#'
#'   * `X()` is placed around a term to define as an independent exposure,
#'   which will be placed in separate formulas from any other term marked as an
#'   exposure
#'
#'   * `F()` is placed around a term for any predictors that should be
#'   maintained/fixed in all models, which can include complex terms, such as
#'   mixed effects
#'
#'   Of note, these formula modifiers do not apply to `htest` objects yet.
#'
#'   For example, the equation below describes two independent exposures "x1"
#'   and "x2" that should be conditionally regressed for every level of "z"
#'
#'   \deqn{y ~ X(x1) + X(x2) + x3 + x4 + F((1 | z))}
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
#' @param ... Not currently used
#'
#' @importFrom dplyr mutate
#' @export
expand_formula <- function(formula,
													 combination,
													 ...) {

	# Initial deparsing of variables
	outcomes <-
		gsub(" ", "", unlist(strsplit(deparse(formula[[2]]), "\ \\+\ ")))
	predictors <- labels(stats::terms(formula))
	exposures <- grep("X\\(", predictors, value = TRUE)
	fixed <- grep("F\\(", predictors, value = TRUE)
	covariates <- setdiff(labels(stats::terms(formula)), c(fixed, exposures))

	# Exposure should be primary variable in formula
	if (length(exposures) == 0) {
		exposures <- setdiff(predictors, c(exposures, fixed))[1]
	} else if (length(exposures) > 0) {
		exposures <-
			gsub("\\)", "", gsub("X\\(", "", exposures)) %>%
			paste(outcomes, ., sep = " ~ ") %>%
			lapply(., stats::formula) %>%
			lapply(., stats::terms) %>%
			lapply(., labels) %>%
			unique()
	}

	# Clean up fixed variables, including mixed variables and parantheses
	fixed <- gsub("\\)$", "", gsub("F\\(", "", fixed))
	fixed[grepl("\\|", fixed)] <-
		gsub("\\(", "", gsub("\\)", "", grep("\\|", fixed, value = TRUE)))
	fixed[grepl("\\|", fixed)] <-
		paste0("(", fixed[grepl("\\|", fixed)], ")")

	# Null out unneeded variables
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
				tidyr::expand_grid(exposures = exposures, .) %>%
				mutate(vars = purrr::map2(vars, exposures, ~ c(.y, .x))) %>%
				mutate(vars = purrr::map(vars, ~ na.omit(.x))) %>%
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
				tidyr::expand_grid(exposures = exposures, .) %>%
				mutate(vars = purrr::map2(vars, exposures, ~ c(.y, .x))) %>%
				mutate(vars = purrr::map(vars, ~ na.omit(.x))) %>%
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
				tidyr::expand_grid(exposures = exposures, .) %>%
				mutate(vars = purrr::map2(vars, exposures, ~ c(.y, .x))) %>%
				mutate(vars = purrr::map(vars, ~ na.omit(.x))) %>%
				tidyr::expand_grid(outcomes = outcomes, .)
		}
	)

	# Expand formulas
	tbl <-
		tbl %>%
		mutate(exposures = sapply(exposures, paste, collapse = ", ")) %>%
		mutate(formulas = {
			purrr::map_chr(vars, ~ paste(unlist(.x), collapse = " + ")) %>%
				paste(outcomes, ., sep = " ~ ") %>%
				lapply(., formula)
		})

	# Return
	tbl

}

