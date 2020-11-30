# Ballistic Analysis ====

#' @title Ballistic Analysis of the Hypotheses
#' @description Look at the overall characteristics of the hypotheses, such as
#'   number of regression models and how they will be put together based on the
#'   approach. Is used to evaluate the objects created by `bullet()` and by
#'   `aim()` (which also uses ballistics to generate the appropriate modeling
#'   matrix).
#' @param mark Represents either a `bullet` or `aim` object to be analyzed for
#'   further information or characteristics
#'
#'   - A single __bullet__ object will return a table that is structured for the
#'   proposed analytical approach
#'
#'   - A single __aim__ object, after the `fire()` function, will return a table
#'   that includes further analytical elements. Currently returns the runtime in
#'   miliseconds.
#'
#' @param ... For extensibility
#' @return The function returns values based on what object was given to it.
#'
#'   A `bullet` object returns a tibble with the following columns:
#'
#'   - outcomes: character vector
#'
#'   - model_num: an identifying model number
#'
#'   - formulas: a list of formulas that will be used for modeling
#'
#'   - model_spec: the `parsnip` model specification
#'
#'   An `aim` object can be returned in two different ways.
#'
#'   - If it has not yet been processed, it returns a table with additional
#'   columns identifying potential problems before it can be processed by the
#'   `fire()` function
#'
#'   - If it has been processed by the `fire()` function, it will return
#'   additional columns to the original table that include model statistics,
#'   including a tidied fitted object
#'
#' @examples
#' library(parsnip)
#' f <- mpg + cyl ~ wt + hp + gear
#' lm_mod <- linear_reg() %>% set_engine("lm")
#' b <- bullet(f, exposure = "wt", approach = "parallel", model = lm_mod)
#' ballistics(b)
#' @family ballistics
#' @export
ballistics <- function(mark, ...) {
	UseMethod("ballistics")
}

#' @export
#' @rdname ballistics
ballistics.default <- function(mark, ...) {
  stop("`ballistics()` is not defined for a '", class(mark)[1], "'.", call. = FALSE)
}

#' @importFrom dplyr mutate
#' @export
#' @rdname ballistics
ballistics.bullet <- function(mark, ...) {

	# This is for a bullet
	bullet <- mark

	# Major variables
	out <- bullet$outcomes
	exp <- bullet$exposures
	pred <- bullet$predictors
	approach <- bullet$approach
	num <- length(bullet$predictors)
	model <- bullet$model

	# Based on approach
	switch(
		approach,
		sequential = {
			tbl <-
				tibble::tibble(model_num = 1:num) %>%
				#mutate(predictors = purrr::map(model_num, ~ c(exp, covar[1:.]))) %>%
				mutate(predictors = purrr::map(model_num, ~ pred[1:.])) %>%
				tidyr::expand_grid(outcomes = out, .)
		},
		parallel = {
			tbl <-
				tibble::tibble(model_num = 1:num) %>%
				mutate(
					predictors = purrr::map(model_num, ~ unique(c(exp, pred[.])))
				) %>%
				tidyr::expand_grid(outcomes = out, .)
		}
	)

	# Now can re-create appropriate formulas
	tbl <-
		tbl %>%
		mutate(formulas = purrr::map_chr(predictors, ~paste(unlist(.x), collapse = " + "))) %>%
		mutate(formulas = paste(outcomes, formulas, sep = " ~ ")) %>%
		mutate(formulas = purrr::map(formulas, ~formula(.x))) %>%
		mutate(model_spec = list(model))

	# Return
	return(tbl)

}

#' @export
#' @rdname ballistics
ballistics.aim <- function(mark, ...) {

	# Using aims instead
	aim <- mark

	# Identify if that has been run or not
	fired <- "fit" %in% names(aim)

	# Add run-time
	if (fired) {
		message("Aimed and fired. Model statistics have been added.")
		aim <-
			aim %>%
			dplyr::mutate(runtime = purrr::map_dbl(fit, function(x) {
				signif(1000 * (x$elapsed[1] + x$elapsed[3]), digits = 2)
			})) %>%
			dplyr::mutate(tidy = purrr::map(fit, ~tidy(.x, exponentiate = TRUE, conf.int = 0.95))) %>%
			new_aim()
	} else {
		message("Putting the aim in sight. Checking if models can be fit.")
		aim <-
			aim %>%
			dplyr::mutate(checkpoint = purrr::map_lgl(model_spec, function(x) {

				# Seeing if model spec is missing data
				status <-
					x %>%
					parsnip::varying_args() %>%
					dplyr::filter(varying == TRUE) %>%
					nrow()

				# If its not, then the test should work
				if (status == 0) {return(TRUE)} else {return(FALSE)}
			})) %>%
			new_aim()

	}

	return(aim)
}
