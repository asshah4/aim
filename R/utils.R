# EXTERNAL ----

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' @importFrom rlang :=
#' @export
rlang::`:=`

#' Create a "fail-safe" of tidying fits
#' @noRd
my_tidy <- function(x,
										conf.int = TRUE,
										conf.level = 0.95,
										exponentiate = TRUE,
										...) {
	broom::tidy(x, conf.int, conf.level, exponentiate)
}
possible_tidy <- purrr::possibly(my_tidy, otherwise = NA, quiet = FALSE)

#' Create a "fail-safe" execution of fit to continue running models
#' @noRd
my_parsnip_fit <- function(...) {
	parsnip::fit.model_spec(...)
}
possible_parsnip_fit <- purrr::possibly(my_parsnip_fit, otherwise = NA, quiet = FALSE)

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
#' @family helpers
#' @export
fit_parsnip_models <- function(.formula, .test, .opts = NULL, .data) {

	purrr::map(.formula, ~ possible_parsnip_fit(.test, .x, .data))

}

#' @description Create a "fail-safe" execution of hypothesis testing
#' @family helpers
#' @noRd
possible_call <- purrr::possibly(do.call, otherwise = NA, quiet = FALSE)

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
#' @family helpers
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
#' @inheritParams broom::tidy.glm
#' @family helpers
#' @export
tidy_tests <- function(.fits,
											 conf.int = TRUE,
											 conf.level = 0.95,
											 exponentiate = TRUE) {

	purrr::map(.fits, ~ broom::tidy(.x, conf.int, conf.level, exponentiate))

}


#' Flatten a Model Map
#'
#' Takes a model that has had results extracted in nested lists, such as
#' __glance__ and __tidy__ and flattens the table into one level, such that the
#' nested tables are expanded out. This is essentially a wrapper for
#' [tidyr::unnest()].
#'
#' @return Returns a `tibble` object, without the attributes/features of a
#'   `model_map` object
#'
#' @inheritParams extract_results
#'
#' @param check Internal usage to turn off validation for work inside other functions
#'
#' @family extractors
#' @export
flatten <- function(model_map, check = TRUE) {

	validate_class(model_map, "model_map")

	if (check) {
		validate_stage(model_map, "extract")
	}

	# Return
	model_map %>%
		subset(., select = -c(formulae, fit)) %>%
		tidyr::unnest(cols = dplyr::any_of(c("tidy", "glance")),
									names_repair = ~ make.unique(., sep = "_"))
}

# INTERNAL ----

#' Return type of test classification
type_of_test <- function(x) {

	if ("model_spec" %in% class(x)) {
		y <- "parsnip"
	}

	if ("function" %in% class(x)) {
		package <- environmentName(environment(x))
		if (package %in% c("stats", "survival")) {
			y <- "stats"
		}
	}

	# Return if either from parsnip or from stats package
	y

}

#' Return name of column from either _character_ or _formula ~ list_ pattern
return_cols <- function(x) {

	if ("character" %in% class(x)) {
		x_col <- as.symbol(x)
	}

	if ("formula" %in% class(x)) {
		x_col <- x[[2]]
	}

	x_col

}

#' Return important rows from either _character_ or _formula ~ list_ pattern
return_rows <- function(x) {

	if ("formula" %in% class(x)) {

		x_list <- eval(x[[3]])

		x_vars <-
			lapply(seq_along(x_list), function(y) {
				if (is.null(names(x_list[y]))) {
					x_list[[y]]
				} else if (names(x_list[y]) == "") {
					x_list[[y]]
				} else {
					names(x_list[y])
				}
			}) |>
			unlist()

	}
	else if ("character" %in% class(x)) {
		x_vars <- lapply(seq_along(x), function(y) {
			if (is.null(names(x[y]))) {
				x[y]
			} else if (names(x[y]) == "") {
				x[y]
			} else {
				names(x[y])
			}
		}) %>%
			unlist()
	}

	x_vars

}

#' Return the names from either _character_ or _formula ~ list_ pattern
return_names <- function(x) {

	if ("formula" %in% class(x)) {
		x_names <-
			x[[3]] |>
			eval() |>
			unlist() |>
			unname()
	}
	else if ("character" %in% class(x)) {
		x_names <- unname(x)
	}

	x_names

}
