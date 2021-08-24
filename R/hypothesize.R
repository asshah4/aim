#' Draw A Hypothesis
#'
#' `hypothesis()` creates a `hypothesis` object that can be placed into a
#' modeling framework. This framework allows for multiple hypotheses to be
#' modeled, and allows for assessment of confounding variables.
#'
#' @return An object of class `hypothesis`
#'
#' @param h General variable that can be cast into a hypothesis object. Currently
#'   this accepts the following object types:
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
#'   `list(name = argument)` pairs.
#'
#' @param data Data set to be linked to this hypothesis. It assumes data is one
#'   of type `c("tbl_df", "tbl", "data.frame")`. The name of the data set is
#'   taken from the object itself.
#'
#' @param strata How the data should be split or stratified. References the
#'   name of the data given in that the models will be with fit against,
#'   splitting the data into subsets. This helps to perform hypothesis testing
#'   on subsets or strata of the data. It defaults to NULL (which means the full
#'   data will be used). This argument will only be incorporated if `data` is
#'   present.
#'
#' @param ... For additional variables based on the generic method invoked.
#'
#' @section Warning:
#'
#'   The formula object does not necessarily work for `htest` objects as it is
#'   still __experimental__.
#'
#' @inheritSection expand_formula Formulae
#'
#' @rdname hypothesize
#' @export
hypothesize <- function(h, ...) {
	UseMethod("hypothesize", object = h)
}

#' @rdname hypothesize
#' @export
hypothesize.formula <- function(h,
																combination,
																test,
																test_opts = NULL,
																data,
																strata = NULL,
																...) {

	# Check for test_opts
	if (is.null(test_opts)) {test_opts <- NA}

	# Check for strata
	if (is.null(strata)) {strata <- NA}

	# Get data name
	data_name <- deparse(substitute(data))

	# Validate/ensure appropriate test object: TODO

	# Construct
	h <- new_hypothesis(
		hypothesis = h,
		combination = combination,
		test = test,
		test_opts = test_opts,
		data = data,
		data_name = data_name,
		strata = strata
	)

	# Validate
	validate_hypothesis(h)

	# Return
	h

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
													 strata) {

	structure(
		hypothesis,
		combination = combination, # Formula building pattern
		test = test, # Either model_spec or htest object
		test_opts = test_opts, # Additional arguments to pass down
		data = data, # Unnamed data
		data_name = data_name, # Name of data frame
		strata = strata, # Name of variable for stratification
		class = c("hypothesis", class(hypothesis)) # Class definition
	)

}

# Generics ----

#' @inheritParams base::print
#' @export
print.hypothesis <- function(x, ...) {

	# Retrieve variables
	h <- stats::formula(stats::terms(x))
	combination <- attr(x, "combination")
	test <- attr(x, "test")
	data_name <- attr(x, "data_name")

	# Glue message
	cat(glue::glue(
		"
		Hypothesis: {deparse(h)}

		Description:

		Combination	{combination}
		Test		{class(test)[1]}
		Data		{data_name}
		"
	))

	# Return invisibly
	invisible(x)

}
