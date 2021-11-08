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
#' @param x General variable that can be cast into a hypothesis object.
#'   Currently this accepts the following object types:
#'
#'   * `formula` objects, which can be given additional modifying label. The key
#'   variables, as described in the __formulae section__ below, are `exposures`
#'   and `fixed`, as they allow for specific transformations of the formula.
#'   This behavior is also described in [expand_formula()].
#'
#'   * `rx` objects, which are modified formulas created by [rx::rx()] and have
#'   additional labels available. Can be set by [rx::set_rx_theme()] using the
#'   argument `set_rx_theme("epi")`. This behavior is described in
#'   [expand_formula()] or the formula section below with further details.
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
hypothesize <- function(x, ...) {
	UseMethod("hypothesize", object = x)
}

#' @rdname hypothesize
#' @param exposures A character vector for individual exposures, which will
#'   always be placed in separate models.
#' @param fixed A character vector naming variables that should be retained in
#'   all models.
#' @export
hypothesize.formula <- function(x,
																exposures = NULL,
																fixed = NULL,
																combination,
																test,
																test_opts = NA,
																data,
																strata = NA,
																origin = "independent",
																...) {

	# Get data name
	data_name <- deparse(substitute(data))

	# If strata are present, than need to remove from formula
	if (!is.na(strata)) {
		x <- stats::update(x, bquote(. ~ . - .(as.name(strata))))
	}

	# Create labels and check to see if labels are in the terms
	labels <- list(exposures = exposures, fixed = fixed)
	check_labels(x, labels)

	# Validate terms are in data
	check_terms(x, data)

	# Construct
	h <- new_hypothesis(
		hypothesis = x,
		labels = labels,
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
hypothesize.rx <- function(x,
													 combination,
													 test,
													 test_opts = NA,
													 data,
													 strata = NA,
													 origin = "independent",
													 ...) {

	# Rx formula to be extracted
	validate_class(x, "rx")
	labels <-
		attributes(x)$roles %>%
		rx::table_to_list(roles)

	# Left and right hand sides
	lhs <-
		getOption("rx.lhs") %>%
		labels[[.]] %>%
		paste(., collapse = " + ")
	rhs <-
		getOption("rx.rhs") %>%
		labels[[.]]

	# The formula should be simplified with removal of special characters
	new_formula <- stats::reformulate(rhs, lhs)

	# Get data name
	data_name <- deparse(substitute(data))

	# TODO Validate/ensure appropriate test object
	test_type <- type_of_test(test)

	# If strata are present, than need to remove from formula
	if (!is.na(strata)) {
		x <- stats::update(new_formula, bquote(. ~ . - .(as.name(strata))))
	}

	# Validate terms are in data
	check_terms(x, data)

	# Construct
	h <- new_hypothesis(
		hypothesis = x,
		labels = labels,
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
hypothesize.list <- function(x, ...) {
	stop(
		"`hypothesize()` does not yet have full functionality for `list` objects.",
		call. = FALSE
	)
}

#' @rdname hypothesize
#' @export
hypothesize.dagitty <- function(x, ...) {
	stop(
		"`hypothesize()` does not yet have full functionality for `dagitty` objects.",
		call. = FALSE
	)
}

#' @rdname hypothesize
#' @export
hypothesize.tidy_dagitty <- function(x, ...) {
	stop(
		"`hypothesize()` does not yet have full functionality for `tidy_dagitty` objects.",
		call. = FALSE
	)
}

#' @rdname hypothesize
#' @export
hypothesize.recipe <- function(x, ...) {
	stop(
		"`hypothesize()` does not yet have full functionality for `recipe` objects."
	)
}

#' @rdname hypothesize
#' @export
hypothesize.default <- function(x, ...) {
	stop(
		"`hypothesize()` is not defined for a `", class(x)[1], "` object.",
		call. = FALSE
	)
}

#' Constructor for `hypothesis` class
#' @noRd
new_hypothesis <- function(hypothesis,
													 labels,
													 combination,
													 test,
													 test_opts,
													 data,
													 data_name,
													 strata,
													 origin) {

	structure(
		hypothesis,
		labels = labels, # Named list pairs of formula terms
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
	cat("\n")

	# Return invisibly
	invisible(x)

}

