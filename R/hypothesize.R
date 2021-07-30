#' Create a Hypothesis
#'
#' `hypothesize()` creates a `hypothesis` object that can be placed into a
#' modeling framework. This framework allows for multiple hypotheses to be
#' modeled, and allows for assessment of confounding variables.
#'
#' @return An object of class `hypothesis`
#'
#' @param h General object that can be cast into a hypothesis object. Currently
#'   this accepts the following object types:
#'
#'   * `formula` objects, which  be given additional modifiers as described.
#'   Please see [expand_formula()] or the formula section below for details.
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
#' @param .test_opts For optional or additional parameters to be given to the
#'   *test* argument. Should be in format of a list with name-pairs, such as
#'   `list(name = argument)` pairs.
#'
#' @inheritParams add_data
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

#' For general formula `hypothesis` objects
#' @rdname hypothesize
#' @export
hypothesize.formula <- function(h,
																combination,
																test,
																.test_opts = NULL,
																.data = NULL,
																.strata = NULL,
																...) {
	# Initialize a `hypothesis` object
	hypothesis <- new_hypothesis(hypothesis = h)

	# The data name helps to link the datasets to the tests and hypothesis
	if (!is.null(.data)) {
		.data_name <- deparse(substitute(.data))
	} else {
		.data_name <- NA
	}

	# Modify and return attributes
	hypothesis <-
		hypothesis %>%
		.set_hypothesis_formulae(h, combination) %>%
		.set_hypothesis_tests(test, .test_opts) %>%
		.set_hypothesis_data(.data, .data_name, .strata)

	validate_class(hypothesis, "hypothesis")

	# Return
	hypothesis
}

#' Default for `hypothesis` objects
#' @rdname hypothesize
#' @export
hypothesize.default <- function(h, ...) {
	stop(
		"`hypothesize()` is not defined for a `", class(x)[1], "` object.",
		call. = FALSE
	)
}

#' Constructor for `hypothesis` class
#' @noRd
new_hypothesis <- function(hypothesis,
													 combination,
													 formulae,
													 parameters,
													 test,
													 test_opts,
													 data,
													 strata) {
	# Class validation
	validate_class(hypothesis, "formula")

	# Return a new framework object
	structure(
		hypothesis,
		class = c("hypothesis", class(hypothesis)), # Class definition
		combination = character(), # Formula building pattern
		formulae = list(), # Formulae to be built
		parameters = tibble::tibble(), # Parameter table
		test = list(), # Either model_spec or htest object
		test_opts = list(), # Additional arguments to pass down
		data = list(), # List of data frames, named
		strata = list() # List of variables contained in linked data frames
	)
}

