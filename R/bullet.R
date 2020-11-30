# Marksman's Bullets ====

#' @title Create bullets to be loaded
#'
#' @description A `bullet` object is a pre-specified hypothesis, with a formula
#'   that helps to identify the individual variables and their roles. It
#'   requires the use of a model specification from `parsnip` (or any other
#'   similarly specified models). It also determines what sequence of models to
#'   put together.
#'
#' @param f Formula showing relationship of outcomes and predictors
#'
#' @param exposure Variable(s) that is forced to be maintained in every model as
#'   a predictor.
#'
#' @param approach The modeling approach that will be used. The options are:
#'
#'   * `sequential` will build y ~ x1, y ~ x1 + x2 models
#'
#'   * `parallel` will build y ~ x1, y ~ x2 models
#'
#' @param model A model specified by the [`parsnip`
#'   package](https://parsnip.tidymodels.org/articles/parsnip_Intro.html), which
#'   includes the mode and computational engine
#'
#' @param ... For extensibility
#'
#' @return Object of type `bullet` that contains the hypothesis and the
#'   characteristics of how to put the analysis together. needed for analysis
#'
#' @examples
#' library(parsnip)
#' f <- mpg + cyl ~ wt + hp + gear
#' lm_mod <- linear_reg() %>% set_engine("lm")
#' b <- bullet(f, exposure = "wt", approach = "sequential", model = lm_mod)
#'
#' @importFrom magrittr %>%
#'
#' @export
bullet <- function(f, exposure = NULL, approach = "sequential", model, ...) {

	# Break apart formula
	outcomes <- all.vars(f[[2]])
	predictors <- all.vars(f[[3]])
	exposures <- exposure
	covariates <- setdiff(predictors, exposures)

	# Exposure should always be first variables
	predictors <- c(exposures, covariates)

	# Items that will be loaded
	shell <- list(
		outcomes = outcomes,
		predictors = predictors,
		exposures = exposures,
		covariates = covariates,
		approach = approach,
		model = model
	)

	# Constructor function being used
	shell <- new_bullet(shell)

	# Return
	return(shell)
}

#' @description Construct a new bullet, with validation built in
#' @noRd
new_bullet <- function(shell) {

	# Confirm that the "shell" is a list of bullet characteristics
	stopifnot(is.list(shell))

	# Final structure defined
	structure(shell, class = "bullet")

}
