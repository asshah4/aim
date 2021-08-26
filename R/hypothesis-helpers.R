# Setting Functions ----

#' Set Components of a Hypothesis
#'
#' @description
#'
#' The following functions will replace the internal attributes of a
#' `hypothesis` object, without warning.
#'
#' * `set_formulae()` expands the formula based on outcomes and predictors
#' (including both exposures, covariates, and fixed variables). The pattern is
#' decided by the `combination` argument. The major functionality is within the
#' `expand_formula()` function, but this allows for setting internal attributes.
#'
#' * `set_data()` adds data to a formula (internal to the `hypothesize()`
#' function), along with potential stratification instructions. Modifies the
#' formula by adding the attributes to the formula.
#'
#' * `set_tests()` Add the test and test options (if needed) the the
#' `hypothesis` object. This modifies the attributes of the original object and
#' invisibly returns the same object.
#'
#' @return Invisibly returns the `hypothesis` object
#'
#' @param hypothesis A `hypothesis` object
#' @param combination The pattern of how to generate formulas
#' @param data A data frame object
#' @param data_name Abstracted from the **data** argument
#' @param strata Optional argument to subset data frame
#' @name modify_hypothesis
#' @keywords internal
NULL

#' @rdname modify_hypothesis
#' @keywords internal
set_formulae <- function(hypothesis, formula, combination) {
	# Validate class
	validate_class(formula, "formula")
	validate_class(combination, "character")

	tbl <- expand_formula(formula, combination, table = TRUE)
	formulae <- tbl$formulae
	parameters <- tbl[1:4]

	attributes(hypothesis)$combination <- combination
	attributes(hypothesis)$formulae <- formulae
	attributes(hypothesis)$parameters <- parameters

	# Return
	invisible(hypothesis)
}

#' @rdname modify_hypothesis
#' @keywords internal
set_data <- function(hypothesis, data, data_name, strata = NULL) {

	# Validate if present
	validate_class(data, c("data.frame", "tbl", "tbl_df"))

	# If a data frame, and make sure data slot is empty
	if (inherits(data, "data.frame")) {
		attributes(hypothesis)$data[[data_name]] <- data
	}

	# Strata, should also be empty prior to starting
	if (is.null(strata)) {
		attributes(hypothesis)$strata[[data_name]] <- NA
	} else if (inherits(strata, "character")) {
		attributes(hypothesis)$strata[[data_name]] <- strata
	}

	# Return formula/hypothesis object
	invisible(hypothesis)
}

#' @rdname modify_hypothesis
#' @keywords internal
set_tests <- function(hypothesis, test, .test_opts) {

	# Validate classes
	validate_class(test, c("model_spec", "htest"))

	# Set attributes
	attributes(hypothesis)$test <- test
	attributes(hypothesis)$test_opts <- .test_opts

	# Return
	invisible(hypothesis)
}

#' @rdname modify_hypothesis
#' @keywords internal
set_vars <- function(hypothesis, formula, confounders = NULL) {

	# Outcomes
	outcomes <- gsub(" ", "", unlist(strsplit(deparse(formula[[2]]), "\ \\+\ ")))
	if (length(outcomes) == 0) {outcomes <- NA}
	attributes(hypothesis)$vars$outcomes <- outcomes

	# Predictors
	predictors <- labels(stats::terms(formula))
	exposures <- grep("X\\(", predictors, value = TRUE)
	fixed <- grep("F\\(", predictors, value = TRUE)
	covariates <- setdiff(predictors, c(fixed, exposures))

	# Clean variables are needed
	covariates <- gsub("\\)", "", gsub("C\\(", "", covariates))
	if (length(covariates) == 0) {covariates <- NA}
	exposures <- gsub("\\)", "", gsub("X\\(", "", exposures))
	if (length(exposures) == 0) {exposures <- NA}
	fixed <- gsub("\\)", "", gsub("F\\(", "", fixed))
	if (length(fixed) == 0) {fixed <- NA}

	# Assign back to hypothesis
	attributes(hypothesis)$vars$exposures <- exposures
	attributes(hypothesis)$vars$fixed <- fixed
	attributes(hypothesis)$vars$covariates <- covariates

	# Confounders need to be identified
	if (is.null(confounders)) {
		confounders <- grep("C\\(", predictors, value = TRUE)
		confounders <- gsub("\\)", "", gsub("C\\(", "", confounders))
		if (length(confounders) == 0) {confounders <- NA}
		attributes(hypothesis)$vars$confounders <- confounders
	}

	# Return
	invisible(hypothesis)
}


# Updating Functions ----

#' Update a Hypothesis
#'
#' A `hypothesis` object can be modified to generate an updated hypothesis,
#' which can then be placed into a `framework` as needed. The **strata** and
#' **test_opts** arguments can only be updated if the **data** and **test**
#' options are also given, respectively.
#'
#' @return A `hypothesis` object
#'
#' @param hypothesis A `hypothesis` object
#' @param ... Additional named arguments to pass (should be named components of
#'   a `hypothesis` object)
#'
#' @export
update_hypothesis <- function(hypothesis, ...) {

	mc <- match.call(expand.dots = TRUE)

	changes <- list(...)

	new_names <- names(changes)
	old_names <- names(attributes(hypothesis))

	for (i in new_names) {
		# Ensure appropriate arguments
		if (!(i %in% old_names)) {
			stop("The argument `",
					 i,
					 "` is not a valid argument for a `hypothesis` object")
		}

		# Reexpand formulas if updating combination
		if (i == "combination") {
			h <- stats::formula(stats::terms(hypothesis))
			hypothesis <- hypothesis %>% set_formulae(h, changes[[i]])
		}
		# Old data should be removed before placing new data
		else if (i == "data") {
			attributes(hypothesis)$data <- changes[[i]]
			new_data_name <- mc[[i]]
			attributes(hypothesis)$data_name <- new_data_name
		}
		# Replace the old with the new
		else {
			attributes(hypothesis)[[i]] <- changes[[i]]
		}
	}

	invisible(hypothesis)

}
