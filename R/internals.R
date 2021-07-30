# Framework ----

#' Link Hypothesis to Data
#'
#' Sets the status of the `framework` object to tie the hypothesis to the data
#' it will be tested against.
#' @keywords internal
.link_hypothesis_to_data <- function(framework, hypothesis, name) {

	# If data is present in the hypothesis
	attributes(framework)$data_table <-
		attributes(framework)$data_table %>%
		tibble::add_row(
			hypothesis_name = name,
			data_name = names(attributes(hypothesis)$data),
			data = attributes(hypothesis)$data,
			strata = attributes(hypothesis)$strata
		)

	# Return framework
	framework
}

# Hypothesis ----

#' Set Formulas in a Hypothesis
#'
#' Expand the formula based on outcomes and predictors (including both
#' exposures, covariates, and fixed variables). The pattern is decided by the
#' `combination` argument. The major functionality is within the
#' `expand_formula()` function, but this allows for setting internal attributes.
#' @keywords internal
.set_hypothesis_formulae <- function(hypothesis, formula, combination) {
	# Validate class
	validate_class(formula, "formula")
	validate_class(combination, "character")

	tbl <- expand_formula(formula, combination)
	formulae <- tbl$formulae
	parameters <- tbl[1:4]

	attributes(hypothesis)$combination <- combination
	attributes(hypothesis)$formulae <- formulae
	attributes(hypothesis)$parameters <- parameters

	# Return
	invisible(hypothesis)
}

#' Set Data in a Hypothesis
#'
#' Add data to a formula (internal to the `hypothesize()` function), along with
#' potential stratification instructions. Modifies the formula by adding the
#' attributes to the formula. Data argument can either be data frame or a
#' character vector referring to a data set that will be added later.
#' @keywords internal
.set_hypothesis_data <- function(hypothesis, .data, .data_name, .strata) {

	if (is.null(.data)) {
		warning(
			"The `.data` object provided is NULL, and this hypothesis cannot be tested until this is added, such as with the `add_data()` function."
		)
	}

	if (!is.null(.data)) {
		# Validate if present
		validate_class(.data, c("data.frame", "tbl", "tbl_df", "character"))

		# If a data frame
		if (inherits(.data, "data.frame")) {
			attributes(hypothesis)$data[[.data_name]] <- .data
		}

		# If a name for a data frame
		if (inherits(.data, "character")) {
			data_name <- .data
			attributes(hypothesis)$data[[.data_name]] <- NA
			message(
				"The `.data` variable is a character, thus a data set must be added in a subsequent step that matches this name."
			)
		}

		# Strata
		if (is.null(.strata)) {
			attributes(hypothesis)$strata[[.data_name]] <- NA
		} else if (inherits(.strata, "character")) {
			attributes(hypothesis)$strata[[.data_name]] <- .strata
		}
	}

	# Return formula/hypothesis object
	invisible(hypothesis)
}

#' Set Test and Test Options in a Hypothesis
#'
#' Add the test and test options (if needed) the the `hypothesis` object. This modifies the attributes of the original object and invisibly returns the same ojbect.
#' @keywords internal
.set_hypothesis_tests <- function(hypothesis, test, .test_opts) {

	# Validate classes
	validate_class(test, c("model_spec", "htest"))

	# Set attributes
	attributes(hypothesis)$test <- test
	attributes(hypothesis)$test_opts <- .test_opts

	# Return
	invisible(hypothesis)
}
