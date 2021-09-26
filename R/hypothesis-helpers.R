#' Expand Formulae for a Hypothesis
#'
#' This is an internal function that helps to create the appropriate formula
#' combinations for adding hypotheses.
#'
#' @return List or table of formulae and extracted terms (to be determined by
#'   the __table__ argument).
#'
#' @param formula Formula showing relationship of outcomes and predictors, and
#'   is essentially the hypothesis. It allows for complex formulae e.g. multiple
#'   predictors, multiple exposures, and multiple covariates, that can be
#'   organized into individual formulae based on additional modifiers. Please
#'   see the  for further documentation.
#'
#' @param labels A named list pair of term labels and a character vector of
#'   terms, such as as `list(exposure = "x1", outcome = "y")`
#'
#' @param combination The building pattern for how to put together the overall
#'  plan. It defines variable relationships that will be used. The options of
#'  `combination` will lead to the following patterns:
#'
#'  * `direct` will not modify the original equation pattern
#'
#'  \deqn{y ~ x1 + x2}
#'
#'  * `sequential` will create a list of sequentially adjusted formula
#'
#'  \deqn{y ~ x1} \deqn{y ~ x1 + x2}
#'
#'  * `parallel` will create a list of formula with parallel predictors
#'
#'  \deqn{y ~ x1} \deqn{y ~ x2}
#'
#' @param table Determines if the returned value should be a list of formulas or
#'   a table. Defaults to FALSE, leading to a list generation.
#'
#' @param ... Not currently used
#'
#' @section Formulae:
#'
#' For each `formula`, the terms in the RHS and LHS can be labeled. Each individual LHS term is considered an individual outcome, and each RHS is a predictor which can be grouped in a variety of ways. If labels are provided, then the `formula` will be expanded in a certain pattern. The key labels are:
#'
#'   * __exposures__ = defines an independent exposure, which will be placed in a separate formulae from any other term marked as such
#'
#'   * __fixed__ = defines any predictor that should be maintained/fixed in all models, including complex terms such as mixed/random effects
#'
#' @importFrom dplyr mutate
#' @export
expand_formula <- function(formula,
													 labels = NULL,
													 combination,
													 table = FALSE,
													 ...) {
	# Initial deparsing of variables
	outcomes <-
		gsub(" ", "", unlist(strsplit(deparse(formula[[2]]), "\ \\+\ ")))
	predictors <- labels(stats::terms(formula))
	exposures <- labels[["exposures"]]
	fixed <- labels[["fixed"]]
	confounders <- labels[["confounders"]]

	# Covariates should not contain any additional labels
	covariates <- setdiff(labels(stats::terms(formula)), c(fixed, exposures))
	covariates <- gsub("\\)", "", gsub("C\\(", "", covariates))

	if (length(exposures) == 0) {
		exposures <- NA
	}

	if (length(fixed) == 0) {
		fixed <- NULL
	}

	# Reset the covariates and ensure fixed variables are primary/upfront
	covariates <- setdiff(covariates, c(fixed, exposures))
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
			mod <- unique(!is.na(exposures))
			num <- sum(mod + length(covariates))

			tbl <-
				tibble::tibble(number = 1:num) %>%
				mutate(vars = purrr::map(
					number,
					~ c(fixed, covariates[(1 - mod):(.x - mod)])
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
				num <- sum(any(c(!is.null(fixed), !is.na(exposures))))
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
	tbl <- tbl %>%
		mutate(exposures = sapply(exposures, paste, collapse = ", ")) %>%
		mutate(formulae = {
			purrr::map_chr(vars, ~ paste(unlist(.x), collapse = " + ")) %>%
				paste(outcomes, ., sep = " ~ ") %>%
				lapply(., formula)
		})

	# Return either list or table
	if (table) {
		tbl
	} else {
		tbl$formulae
	}
}

#' Update a Hypothesis
#'
#' A `hypothesis` object can be modified to generate an updated hypothesis,
#' which can then be placed into a `framework` as needed. The **strata** and
#' **test_opts** arguments can only be updated if the **data** and **test**
#' options are also given, respectively.
#'
#' @return A `hypothesis` object
#'
#' @param hypothesis A `hypothesis` object
#' @param ... Additional named arguments to pass (should be named components of
#'   a `hypothesis` object)
#'
#' @export
update_hypothesis <- function(hypothesis, ...) {

	mc <- match.call(expand.dots = TRUE)

	changes <- list(...)

	new_names <- names(changes)
	old_names <- names(attributes(hypothesis))

	for (i in new_names) {
		# Ensure appropriate arguments
		if (!(i %in% old_names)) {
			stop("The argument `",
					 i,
					 "` is not a valid argument for a `hypothesis` object")
		}

		# Update combination
		if (i == "combination") {
			attributes(hypothesis)$combination <- changes[[i]]
		}
		# Old data should be removed before placing new data
		else if (i == "data") {
			attributes(hypothesis)$data <- changes[[i]]
			new_data_name <- mc[[i]]
			attributes(hypothesis)$data_name <- as.character(new_data_name)
		}
		# Modify formula if strata is new
		else if (i == "strata") {
			a <- attributes(hypothesis)
			hypothesis <-
				stats::update(hypothesis, bquote(. ~ . - .(as.name(changes[[i]]))))
			attributes(hypothesis) <- a
			attributes(hypothesis)$strata <- changes[[i]]
		}
		# Replace the old with the new
		else {
			attributes(hypothesis)[[i]] <- changes[[i]]
		}
	}

	invisible(hypothesis)

}
