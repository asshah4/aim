#' Add Arms to the `octomod`
#'
#' Add a new hypothesis arm to the `octomod` object. The hypothesis collected
#' here are all directed at the core dataset specified by the `core()`
#' function.
#'
#' This step is the most crucial, in that the the hypothesis and relationship
#' between variables is defined at this stage.
#'
#' @return An `octomod` object with arms attached
#'
#' @param octomod Object of class `octomod`
#'
#' @param title Character string to identify this arm. Must be unique and not
#'   previously specified.
#'
#' @param plan Formula showing relationship of outcomes and predictors, and is
#'   the essentially the hypothesis. As it allows for complex formulas (e.g.
#'   multiple outcomes), it is referred to as the `plan` instead.
#'
#' @param exposure Variable(s) that are forced to be maintained in every model as
#'   a predictor.
#'
#' @param pattern The building pattern for how to put together the overall plan.
#'   It defines variable relationships that will be used. The options for the
#'   `pattern` currently include:
#'
#'   * `direct` will define the relationship as y ~ x
#'
#'   * `sequential` will define the relationship as y ~ x1, y ~ x1 + x2
#'
#'   * `parallel` will define the relationship as y ~ x1, y ~ x2
#'
#' @param approach The testing approach that should be used, defined as how the
#'   analysis or hypothesis testing should be performed. There are several
#'   options on which specification to use, anywhere from regression modeling to
#'   inferential statistics. The approach applies to the entirety of the growing
#'   arm.
#'
#'   * A model specified by the [`parsnip`
#'   package](https://parsnip.tidymodels.org), which includes the mode and
#'   computational engine
#'
#'   * A statistical test, such as a `t.test`, which may require additional
#'   parameters. These can be given as additional, unmatched arguments. This
#'   option currently supports only hypothesis tests, of class `htest`.
#'
#' @param split How the data should be split. References the name of the
#'   variable in the `core` data that the models will be with fit against,
#'   splitting the data into subsets. This helps to perform hypothesis testing
#'   on subsets or strata of the data. It defaults to NULL (which means the full
#'   data will be used) **experimental**
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
#'   core(mtcars) %>%
#'   arm(
#'     title = "Horsepower",
#'     plan = hp ~ wt,
#'     pattern = "direct",
#'     approach = "t.test",
#'     paired = TRUE,
#'     split = "carb"
#'   )
#'
#' @importFrom dplyr mutate
#' @importFrom tibble tibble
#' @importFrom purrr map
#' @export
#' @rdname arm
arm <- function(octomod, title, plan, exposure = NULL, pattern = "direct", approach, split = NULL, ...) {

	validate_new_arm(octomod, title, plan, exposure, pattern, approach, split)

	# Match call
	mc <- match.call()
	dots <- rlang::dots_list(...)

	# Save additional parameters
	if (length(dots) == 0) {
		pars <- NULL
	} else {
		pars <- dots
	}

	# Break apart formula (deparsing to help with Surv objects)
	out <- gsub(" ", "", unlist(strsplit(deparse(plan[[2]]), "\ \\+\ ")))
	pred <- gsub(" ", "", unlist(strsplit(deparse(plan[[3]]), "\ \\+\ ")))
	exp <- exposure
	covar <- setdiff(pred, exp)

	# Exposure should always be first variables
	pred <- c(exp, covar)

	# Return type of approach, whether model or test class
	type <- type_of_approach(approach)

	# "Regenerate" the lost arm if the approach is not function-like
	if (type == "htest") {
		approach <- generate_new_function(approach)
	}

	# Number of tests per outcome is number of covariates +/- exposure x 1
	num <- length(covar) + !is.null(exp)

	# If exposure are present...
	if (is.null(exp)) {
		nexp <- 1
	} else {
		nexp <- length(exp)
	}

	# Grouping variable is based on `core` data
	if (!is.null(split)) {
		split_level <- unique(dplyr::pull(octomod$core, split))
	} else {
		split_level <- NULL
	}

	# Based on approach
	switch(
		pattern,
		direct = {
			tbl <-
				tibble(test_num = 1:length(out)) %>%
				mutate(vars = list(pred[1:num])) %>%
				mutate(outcomes = out) %>%
				dplyr::relocate(outcomes)
		},
		sequential = {
			tbl <-
				tibble(test_num = 1:num) %>%
				mutate(
					vars = map(
						test_num,
						~ unique(c(exp, pred[nexp:(nexp + .x - 1)]))
					)
				) %>%
				tidyr::expand_grid(outcomes = out, .)
		},
		parallel = {
			tbl <-
				tibble(test_num = 1:num) %>%
				mutate(
					vars = map(test_num, ~ c(exp, covar[.x - 1 + is.null(exp)]))
				) %>%
				tidyr::expand_grid(outcomes = out, .)
		}
	)

	# Now can re-create appropriate formulas, expanding for splits
	arm <-
		tbl %>%
		mutate(formulas = purrr::map_chr(vars, ~paste(unlist(.x), collapse = " + "))) %>%
		mutate(formulas = paste(outcomes, formulas, sep = " ~ ")) %>%
		mutate(formulas = map(formulas, ~formula(.x))) %>%
		mutate(
			approach = list(approach),
			type = type,
			pars = list(pars)
		) %>%
		tidyr::expand_grid(., split_level) %>%
		mutate(split = split)

	# Temporarily create split column if it doesn't exist
	if (!"split" %in% names(arm)) {
		arm <- mutate(arm, split = NA)
	}

	# Add to octomod
	octomod[["arms"]][[title]] <- arm

	# Return
	new_octomod(octomod)

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

#' @description Regenerate a new function from the approach if needed. Should
#'   only be called if the approach is not already a model call (e.g. parsnip
#'   model).
#' @noRd
generate_new_function <- function(approach) {

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
