#' Expand Formulae for a Hypothesis
#'
#' This is an internal function that helps to create the appropriate formula
#' combinations for adding hypotheses.
#'
#' @return Table of formulae and extracted terms
#'
#' @param formula Formula showing relationship of outcomes and predictors, and
#'   is essentially the hypothesis. It allows for complex formulae e.g. multiple
#'   predictors, multiple exposures, and multiple covariates, that can be
#'   organized into individual formulae based on additional modifiers. Please
#'   see the  for further documentation.
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
#' @section Formulae:
#'
#'   When supplying a `formula` object, each RHS term of the formula is
#'   considered an outcome variable, and is analyzed as a single outcome. Each
#'   LHS term is consider a predictor, and can be modified as below:
#'
#'   * `X()` is placed around a term to define as an independent exposure, which
#'   will be placed in separate formulae from any other term marked as an
#'   exposure
#'
#'   * `F()` is placed around a term for any predictors that should be
#'   maintained/fixed in all models, which can include complex terms, such as
#'   mixed effects
#'
#'   For example, the equation below describes two independent exposures "x1"
#'   and "x2" that should be conditionally regressed for every level of "z"
#'
#'   \deqn{y ~ X(x1) + X(x2) + x3 + x4 + F((1 | z))}
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

	# Return expanded formulae
	tbl %>%
		mutate(exposures = sapply(exposures, paste, collapse = ", ")) %>%
		mutate(formulae = {
			purrr::map_chr(vars, ~ paste(unlist(.x), collapse = " + ")) %>%
				paste(outcomes, ., sep = " ~ ") %>%
				lapply(., formula)
		})
}

