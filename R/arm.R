#' Add Arms to the `octomod`
#'
#' Add a new hypothesis arm to the `octomod` object. The hypothesis collected
#' here are all directed at the core dataset specified by the `add_core()`
#' function.
#'
#' This step is the most crucial, in that the the hypothesis and relationship
#' between variables is defined at this stage.
#'
#' @return An `octomod` object with arms attached
#'
#' @param octomod Object of class `octomod`
#'
#' @param title Character string to identify this arm
#'
#' @param f Formula showing relationship of outcomes and predictors, and is the essentially the hypothesis.
#'
#' @param exposure Variable(s) that are forced to be maintained in every model as
#'   a predictor.
#'
#' @param pattern The variable relationship approach that will be used. The
#'   options for the `pattern` are:
#'
#'   * `direct` will define the relationship as y ~ x
#'
#'   * `sequential` will define the relationship as y ~ x1, y ~ x1 + x2
#'
#'   * `parallel` will define the relationship as y ~ x1, y ~ x2
#'
#' @param approach This describes the analysis plan that should be used. There
#'   are several options on which specification to use, anywhere from regression
#'   modeling to inferential statistics. The approach applies to the entirety of
#'   the growing arm.
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
#'   given to the `approach` argument, such as `paired = TRUE` for `t.test`. The
#'   additional parameters must be named to allow them to be passed
#'   successfully.
#'
#' @examples
#' library(magrittr)
#'
#' om <-
#'   octomod() %>%
#'   add_core(mtcars) %>%
#'   add_arm(
#'     title = "Horsepower",
#'     f = disp ~ hp,
#'     pattern = "direct",
#'     approach = "t.test",
#'     paired = TRUE
#'   )
#'
#' @export
#' @rdname arm
add_arm <- function(octomod, title = NULL, f = NULL, exposure = NULL, pattern = "direct", approach, ...) {

	# Check if its octomod in pipeline
	if (!inherits(octomod, "octomod")) {
		stop("The argument must inherit from the `octomod` class.")
	}

	# Check if core data is present
	if ("list" %in% class(octomod$core)) {
		warning("As the core has not yet been loaded, cannot check specified formula against data for available columns.")
	}

	# Check if title is appropriate
	if (!is.null(title) && exists(title, octomod[["arms"]])) {
		stop("The names or `title` of an arm should be unique.")
	}

	# Match call
	mc <- match.call()
	dots <- rlang::dots_list(...)

	# Break apart formula
	out <- all.vars(f[[2]])
	pred <- all.vars(f[[3]])
	exp <- exposure
	covar <- setdiff(pred, exp)

	# Exposure should always be first variables
	pred <- c(exp, covar)

	# Return type of approach, whether model or test class
	type <- type_of_approach(approach)

	# Save additional parameters
	if (length(dots) == 0) {
		pars <- NULL
	} else {
		pars <- dots
	}

	# "Regenerate" the lost arm if the approach is not function-like
	if (type == "htest") {
		approach <- generate(approach)
	}

	# Items that will be loaded
	arm <- list(
		out = out,
		pred = pred,
		exp = exp,
		covar = covar,
		pattern = pattern,
		approach = approach,
		type = type,
		pars = pars
	)

	# Add to octomod
	octomod[["arms"]][[title]] <- coil(arm)

	# Return
	octomod

}


#' @description Evaluate test approach and return the type (or throw an error)
#' @noRd
type_of_approach <- function(approach) {

	# Identify model class
	approach_class <- class(approach)

	# If parsnip model
	if ("model_spec" %in% approach_class) {
		type <- "model_spec"
	}

	# For non-parsnip models, currently limited to htest
	if (inherits(approach, "character")) {
		fn <- get(approach)
		if (is.function(fn)) {
			type <- "htest"
		}
	}

	# Return type
	type
}

#' @description Regenerate a new function from the approach if needed. Should only be called if the approach is not already a model call (e.g. parsnip model).
#' @noRd
generate <- function(approach) {

	# Check approach
	if (!inherits(approach, "character")) {
		stop("The `approach` is not a character string.", call. = FALSE)
	}

	# Make function
	fn <- get(approach)

	# Check if actually function
	if (!is.function(fn)) {
		stop("The `approach` is not a function that can be passed on.")
	}

	# Generate new function
	new_fn <- function(x, y, ...) {
		dots <- unlist(list(...))
		eval(rlang::expr(fn(x, y, !!!dots)))
	}

	# Return
	new_fn

}

#' @description Organize the `arm` by coiling it together
#' @importFrom dplyr mutate
#' @noRd
coil <- function(arm) {

	# Major variables
	out <- arm$out
	exp <- arm$exp
	pred <- arm$pred
	approach <- arm$approach
	num <- length(arm$pred)
	pattern <- arm$pattern
	type <- arm$type
	pars <- arm$pars

	# Based on approach
	switch(
		pattern,
		direct = {
			tbl <-
				tibble::tibble(test_num = 1:num) %>%
				mutate(vars = purrr::map(test_num, ~ pred[1:.])) %>%
				tidyr::expand_grid(outcomes = out, .)
		},
		sequential = {
			tbl <-
				tibble::tibble(test_num = 1:num) %>%
				mutate(vars = purrr::map(test_num, ~ pred[1:.])) %>%
				tidyr::expand_grid(outcomes = out, .)
		},
		parallel = {
			tbl <-
				tibble::tibble(test_num = 1:num) %>%
				mutate(
					vars = purrr::map(test_num, ~ unique(c(exp, pred[.])))
				) %>%
				tidyr::expand_grid(outcomes = out, .)
		}
	)

	# Now can re-create appropriate formulas
	tbl <-
		tbl %>%
		mutate(formulas = purrr::map_chr(vars, ~paste(unlist(.x), collapse = " + "))) %>%
		mutate(formulas = paste(out, formulas, sep = " ~ ")) %>%
		mutate(formulas = purrr::map(formulas, ~formula(.x))) %>%
		mutate(
			approach = list(approach),
			type = type,
			pars = list(pars)
		)

	# Return
	tbl
}
