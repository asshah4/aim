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

#' @description Create a "fail-safe" execution of fit to continue running models
#' @family helpers
#' @noRd
possible_parsnip_fit <- purrr::possibly(parsnip::fit.model_spec, otherwise = NA, quiet = FALSE)

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



