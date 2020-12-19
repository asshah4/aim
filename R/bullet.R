#' @title Create Hypothetical Bullets
#'
#' @description A `bullet` object is a pre-specified hypothesis, with a formula
#'   that helps to identify the individual variables and their roles. It also
#'   asks for the how the `outcome ~ exposure` relationship(s) should be
#'   defined. The purpose of this is to simplify the creating of a project,
#'   where the relationships are defined early, and tested later on.
#'
#' @param f Formula showing relationship of outcomes and predictors
#'
#' @param exposure Variable(s) that are forced to be maintained in every model as
#'   a predictor.
#'
#' @param approach The variable relationship approach that will be used. The
#'   options are:
#'
#'   * `plain` will define a relationship as y ~ x (however not necessary that x
#'   predicts y)
#'
#'   * `sequential` will defines relationship as y ~ x1, y ~ x1 + x2
#'
#'   * `parallel` will define relationships as  y ~ x1, y ~ x2
#'
#' @param spec This describes the analysis plan that should be used to
#'   help aim the bullet. There are several options on specifications to use,
#'   anywhere from regression modeling to inferential statistics. The
#'   specification applies to the entire list of bullets being used.
#'
#'   * A model specified by the [`parsnip`
#'   package](https://parsnip.tidymodels.org), which includes the mode and
#'   computational engine
#'
#'   * A statistical test, such as a `t.test`, which may require additional
#'   parameters. These can be given as additional, unmatched arguments. This
#'   option currently supports only hypothesis tests, of class `htest`.
#'
#' @param ... This should reflect the additional parameters that may need to be
#'   given to the `spec` argument, such as `paired = TRUE` for `t.test`
#'
#' @return Object of type `bullet` that contains the hypothesis and the
#'   characteristics of how to put the analysis together.
#'
#' @examples
#' library(parsnip)
#' f <- mpg + cyl ~ wt + hp + gear
#' lm_mod <- linear_reg() %>% set_engine("lm")
#' b <- bullet(f, exposure = "wt", approach = "simple", spec = lm_mod)
#' b <- bullet(f, exposure = "wt", approach = "simple", spec = "t.test", paired = TRUE)
#'
#' @importFrom magrittr %>%
#'
#' @export
bullet <- function(f, exposure = NULL, approach = "simple", spec, ...) {

	# Match call
	cl <- match.call()
	dots <- rlang::dots_list(...)

	# Break apart formula
	out <- all.vars(f[[2]])
	pred <- all.vars(f[[3]])
	exp <- exposure
	covar <- setdiff(pred, exp)

	# Exposure should always be first variables
	pred <- c(exp, covar)

	# Test specification
	spec_type <- eval_spec(spec)
	if (length(dots) == 0) {
		spec_pars <- NULL
	} else {
		spec_pars <- dots
	}

	# Items that will be loaded
	shell <- list(
		out = out,
		pred = pred,
		exp = exp,
		covar = covar,
		approach = approach,
		spec = spec,
		spec_type = spec_type,
		spec_pars = spec_pars
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

#' @description Evaluate test specification and return the type (or throw an error)
#' @noRd
eval_spec <- function(spec) {

	# Identify if either parsnip model or character
	spec_class <- class(spec)

	# If parsnip model or hypothesis testing
	if ("model_spec" %in% spec_class) {
		spec_type <- spec_class[1]
	} else if (is.character(spec)) {
		test_fn <- get(spec)
		if (is.function(test_fn)) {
			spec_type <- "htest"
		}
	}

}
