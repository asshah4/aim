#' Draw A Hypothesis
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' `hypothesis()` creates a `hypothesis` object that can be placed into a
#' modeling framework. This framework allows for multiple hypotheses to be
#' modeled, and allows for assessment of confounding variables.
#'
#' @return An object of class `hypothesis`
#'
#' @param h General variable that can be cast into a hypothesis object.
#'   Currently this accepts the following object types:
#'
#'   * `formula` objects, which  be given additional modifiers as described.
#'   Please see [expand_formula()] or the formula section below for details.
#'
#'   * `list` object, which describes the outcomes, exposures, covariates, fixed
#'   variables, and potential confounders in a named list, which allows for
#'   grouped covariates when modeling. Please see the details section for
#'   further explanation.
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
#'   *test* argument. Should be in format of a list with name-pairs, such as
#'   `list(name = argument)` pairs. Default is NA, indicating there are no test
#'   options to pass along.
#'
#' @param data Data set to be linked to this hypothesis. It assumes data is one
#'   of type `c("tbl_df", "tbl", "data.frame")`. The name of the data set is
#'   taken from the object itself.
#'
#' @param strata How the data should be split or stratified. References the
#'   name of the data given in that the models will be with fit against,
#'   splitting the data into subsets. This helps to perform hypothesis testing
#'   on subsets or strata of the data. It defaults to __NA__ (which means the
#'   full data will be used). This argument will only be incorporated if `data`
#'   is present.
#'
#' @param origin Variable that serves as an attribute to help track the
#'   hypothesis evolution through the study.
#'
#' @param ... For additional variables based on the generic method invoked.
#'
#' @section Warning:
#'
#'   The `hypothesis` object does not yet fully work for function-based
#'   __tests__, such as:
#'
#'   * `htest` objects from functions like [stats::t.test()] and
#'   [stats::cor.test()]
#'
#'   * `glm` and `lm` objects from functions like [stats::lm()] and
#'   [stats::glm()]
#'
#'   The `hypothesis` doesn't have full functionality for the follow objects
#'   yet:
#'
#'   * `dagitty` from [dagitty::dagitty()] or [ggdag::dagify()]
#'
#'   * `tidy_dagitty` from [ggdag::tidy_dagitty()]
#'
#'   * `recipe` from [recipes::recipe()]
#'
#'   * `list` objects
#'
#' @inheritSection expand_formula Formulae
#'
#' @rdname hypothesize
#' @aliases hypothesize hypothesize.default hypothesize.formula hypothesize.list
#'   hypothesize.dagitty hypothesize.tidy_dagitty hypothesize.recipe
#' @family hypotheses
#' @export
hypothesize <- function(h, ...) {
	UseMethod("hypothesize", object = h)
}

#' @rdname hypothesize
#' @export
hypothesize.formula <- function(h,
																combination,
																test,
																test_opts = NA,
																data,
																strata = NA,
																origin = "independent",
																...) {

	# Get data name
	data_name <- deparse(substitute(data))

	# Validate/ensure appropriate test object: TODO

	# If strata are present, than need to remove from formula
	if (!is.na(strata)) {
		h <- stats::update(h, bquote(. ~ . - .(as.name(strata))))
	}

	# Validate terms are in data
	check_terms(h, data)

	# Construct
	h <- new_hypothesis(
		hypothesis = h,
		combination = combination,
		test = test,
		test_opts = test_opts,
		data = data,
		data_name = data_name,
		strata = strata,
		origin = origin
	)

	# Validate
	validate_hypothesis(h)

	# Return
	h

}

#' @rdname hypothesize
#' @export
hypothesize.list <- function(h, ...) {
	stop(
		"`hypothesize()` does not yet have full functionality for `list` objects."
	)
}

#' @rdname hypothesize
#' @export
hypothesize.dagitty <- function(h, ...) {
	stop(
		"`hypothesize()` does not yet have full functionality for `dagitty` objects."
	)
}

#' @rdname hypothesize
#' @export
hypothesize.tidy_dagitty <- function(h, ...) {
	stop(
		"`hypothesize()` does not yet have full functionality for `tidy_dagitty` objects."
	)
}

#' @rdname hypothesize
#' @export
hypothesize.recipe <- function(h, ...) {
	stop(
		"`hypothesize()` does not yet have full functionality for `recipe` objects."
	)
}

#' @rdname hypothesize
#' @export
hypothesize.default <- function(h, ...) {
	stop(
		"`hypothesize()` is not defined for a `", class(h)[1], "` object.",
		call. = FALSE
	)
}

#' Constructor for `hypothesis` class
#' @noRd
new_hypothesis <- function(hypothesis,
													 combination,
													 test,
													 test_opts,
													 data,
													 data_name,
													 strata,
													 origin) {

	structure(
		hypothesis,
		combination = combination, # Formula building pattern
		test = test, # Either model_spec or htest object
		test_opts = test_opts, # Additional arguments to pass down
		data = data, # Unnamed data
		data_name = data_name, # Name of data frame
		strata = strata, # Name of variable for stratification
		origin = origin, # How the hypothesis was generated
		class = c("hypothesis", class(hypothesis)) # Class definition
	)

}

# Generics ----

#' Print a Hypothesis
#' @param x Print a `hypothesis` object
#' @param ... further arguments passed to or from other methods
#' @export
print.hypothesis <- function(x, ...) {

	# Retrieve variables
	h <- strwrap(deparse(stats::formula(stats::terms(x))))
	combination <- attr(x, "combination")
	test <- attr(x, "test")
	data_name <- attr(x, "data_name")
	strata <- attr(x, "strata")

	cat(glue::glue(
		"
		----------
		Hypothesis
		----------
		"
	))
	cat("\n\n")
	cat(glue::glue("{h}"))
	cat("\n\n")
	cat(glue::glue(
		"
		-----------
		Description
		-----------

		Combination  	{combination}
		Test 		{paste0(class(test), collapse = ', ')}
		Data 		{data_name}
		Strata		{if (is.na(strata)) 'none' else strata}
		"
	))

	# Return invisibly
	invisible(x)

}

# Helper Functions ----

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
#'   * `C()` is placed around a term for any predictors that are a potential
#'   confounder, which does not yet change the relationship of how formulas are
#'   built, but is stored for future methods to be used with [hypothesize()]
#'
#'   For example, the equation below describes two independent exposures "x1"
#'   and "x2" that should be conditionally tested for every level of "z"
#'
#'   \deqn{y ~ X(x1) + X(x2) + x3 + x4 + F((1 | z))}
#'
#' @importFrom dplyr mutate
#' @export
expand_formula <- function(formula,
													 combination,
													 table = FALSE,
													 ...) {
	# Initial deparsing of variables
	outcomes <-
		gsub(" ", "", unlist(strsplit(deparse(formula[[2]]), "\ \\+\ ")))
	predictors <- labels(stats::terms(formula))
	exposures <- grep("X\\(", predictors, value = TRUE)
	fixed <- grep("F\\(", predictors, value = TRUE)

	# Confounders need to be identified
	confounders <- grep("C\\(", predictors, value = TRUE)
	confounders <- gsub("\\)", "", gsub("C\\(", "", confounders))

	# Covariates should not contain any additional labels
	covariates <- setdiff(labels(stats::terms(formula)), c(fixed, exposures))
	covariates <- gsub("\\)", "", gsub("C\\(", "", covariates))

	# Exposures should be cleaned from original modifiers if present, or nulled
	if (length(exposures) > 0) {
		exposures <-
			gsub("\\)$", "", gsub("X\\(", "", exposures)) %>%
			paste(outcomes, ., sep = " ~ ") %>%
			lapply(., stats::formula) %>%
			lapply(., stats::terms) %>%
			lapply(., labels) %>%
			unique() %>%
			unlist()
	} else if (length(exposures) == 0) {
		exposures <- NA
	}

	# Fixed variables may included mixed effect objects or objects with
	# parenthesis. They should be modified to maintain the original structure.
	if (length(fixed) > 0) {
		fixed <- gsub("\\)$", "", gsub("F\\(", "", fixed))
		fixed[grepl("\\|", fixed)] <-
			gsub("\\(", "", gsub("\\)", "", grep("\\|", fixed, value = TRUE)))
		fixed[grepl("\\|", fixed)] <-
			paste0("(", fixed[grepl("\\|", fixed)], ")")
	} else if (length(fixed) == 0) {
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
