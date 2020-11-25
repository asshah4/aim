# Marksman's Bullets ====

#' @title Create bullets to be loaded
#' @param f Formula showing relationship of outcomes to predictors
#' @param exposure Variable(s) that is forced to be maintained in every model as a
#'   predictor.
#' @param approach The modeling approach that will be used. The options are:
#'
#'   * `sequential` will build y ~ x1, y ~ x1 + x2 models
#'
#'   * `parallel` will build y ~ x1, y ~ x2 models
#'
#' @param model A model specified by the [`parsnip`
#'   package](https://parsnip.tidymodels.org/articles/parsnip_Intro.html), which
#'   includes the mode and computational engine
#' @param ... For extensibility
#' @examples
#' library(card)
#' library(parsnip)
#' data(geh)
#' f <- svg_mag + qrs_tang ~ lab_hba1c + age + sex + bmi + cad + htn
#' lm_mod <- linear_reg() %>% set_engine("lm")
#' b <- bullet(f, exposure = "lab_hba1c", approach = "sequential", model = lm_mod)
#' @return List of hypothesis characteristics needed for analysis
#' @importFrom magrittr %>%
#' @export
bullet <- function(f, exposure = NULL, approach = "sequential", model, ...) {

	# Break apart formula
	outcomes <- all.vars(f[[2]])
	predictors <- all.vars(f[[3]])
	exposures <- exposure
	covariates <- setdiff(predictors, exposures)

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

# Print Method

#' @description Generic print method
#' @param x Object of class `bullet`
#' @param ... arguments to pass on
#' @noRd
#' @export
print.bullet <- function(x, ...) {

	cat("Hypothetical bullet... \n \n")
	cat("Outcomes: ", length(x$outcomes), "\n")
	cat("Exposures: ", length(x$exposures), "\n")
	cat("Covariates: ", length(x$covariates), "\n")
	cat("Approach: ", length(x$approach), "\n")
	cat("Model: ", x$model$engine, x$model$mode, "\n")

}
