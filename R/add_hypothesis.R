#' Add a Hypothesis to the Project
#'
#' Adding an "arm" to the project reflects a series of hypothesis generated
#' around the core data that was set in the preceding function. The hypothesis
#' formula allows for modifiers to both the formula itself, as well as *how* it
#' should be applied to the data set.
#'
#' @return `project` object
#'
#' @param project Object of class `project`
#'
#' @param name Character string to identify this arm. Must be unique and not
#'   previously specified.
#'
#' @param formula Formula showing relationship of outcomes and predictors, and is
#'   the essentially the hypothesis. As it allows for complex formulas (e.g.
#'   multiple outcomes), it is referred to as the `plan` instead.
#'
#' @param fixed Variable(s) that are forced to be maintained in every model as
#'   a predictor.
#'
#' @param combination The building pattern for how to put together the overall plan.
#'   It defines variable relationships that will be used. The options for the
#'   `combination` currently include:
#'
#'   * `direct` will define the relationship as y ~ x
#'
#'   * `sequential` will define the relationship as y ~ x1, y ~ x1 + x2
#'
#'   * `parallel` will define the relationship as y ~ x1, y ~ x2
#'
#' @param test The testing that will be used, defined as how the
#'   analysis or hypothesis testing should be performed. There are several
#'   options on which specification to use, anywhere from regression modeling to
#'   inferential statistics. The test applies to the collection of hypotheses.
#'
#'   * A model specified by the [`parsnip`
#'   package](https://parsnip.tidymodels.org), which includes the mode and
#'   computational engine
#'
#'   * A statistical test, such as a `t.test`, which may require additional
#'   parameters. These can be given as additional, unmatched arguments. This
#'   option currently supports only hypothesis tests, of class `htest`.
#'
#' @param strata How the data should be split or stratified. References the name
#'   of the data given in `set_data()` that the models will be with fit
#'   against, splitting the data into subsets. This helps to perform hypothesis
#'   testing on subsets or strata of the data. It defaults to NULL (which means
#'   the full data will be used) **experimental**
#'
#' @param ... This should reflect the additional parameters that may need to be
#'   given to the `test` argument, such as `paired = TRUE` for `t.test()`. The
#'   additional parameters must be named to allow them to be passed
#'   successfully.
#'
#' @export
add_hypothesis <- function(project, name, formula, fixed = NULL, combination = "direct", test, which_data = NULL, strata = NULL, ...) {

	# Check data file
	validate_project_data(project)

	# Save additional, optional parameters
	dots <- rlang::dots_list(...)
	if (length(dots) == 0) {
		test_pars <- NULL
	} else {
		test_pars <- dots
	}

	# Ensure correct project data and arm
	row <- which_project_row(project, which_data)
	data <- project$data[[row]]
	arm <- project$hypothesis[[row]]
	type <- type_of_test(test)

	# Set project flags and attributes
	project <- flag_status(project, name, row, strata, type)

	# Formula table
	tbl <- make_formulas(formula, fixed, combination)

	# Add tests and test options
	if (type == "model_spec") {
		tbl$tests <- list(test)
	}
	else if (type == "htest") {
		test <- generate_new_function(test)
		tbl$tests <- list(test)
	}
	tbl$options <- test_pars

	# Add strata if available
	if (!is.null(strata)) {
		tbl <- tidyr::expand_grid(tbl, level = create_strata(data, strata))
	}

	# Add the arm
	arm[[name]] <- tbl

	# Place back into project
	project$hypothesis[[row]] <- arm

	# Return
	project

}

#' @description Make formula combinations in `add_hypothesis()`
#' @noRd
make_formulas <- function(formula, fixed, combination) {

	# Break apart formula (deparsing to help with survival/mixed objects)
	outcomes <-
		gsub(" ", "", unlist(strsplit(deparse(formula[[2]]), "\ \\+\ ")))
	predictors <- labels(stats::terms(formula))
	exposures <- fixed
	exposures[grepl("\\|", exposures)] <-
		gsub("\\(", "", gsub("\\)", "", grep("\\|", exposures, value = TRUE)))
	covariates <- setdiff(predictors, exposures)

	# Need to add parenthesis back to mixed variables
	exposures[grepl("\\|", exposures)] <-
		paste0("(", exposures[grepl("\\|", exposures)], ")")

	# Clean up exposures
	if (length(exposures) == 0) {exposures <- NULL}

	# Exposure should always be first variables
	predictors <- c(exposures, covariates)

	# Number of tests per outcome is number of covariates +/- exposure x 1
	num <- length(covariates) + !is.null(exposures)

	# Determine number of exposures
	if (length(exposures) == 0) {
		nexp <- 1
	} else {
		nexp <- length(exposures)
	}

	# Based on approach
	switch(
		combination,
		direct = {
			tbl <-
				tibble(test_num = 1:length(outcomes)) %>%
				mutate(vars = list(predictors[1:num])) %>%
				mutate(outcomes = outcomes) %>%
				dplyr::relocate(outcomes)
		},
		sequential = {
			tbl <-
				tibble(test_num = 1:num) %>%
				mutate(
					vars = map(
						test_num,
						~ unique(c(exposures, predictors[nexp:(nexp + .x - 1)]))
					)
				) %>%
				tidyr::expand_grid(outcomes = outcomes, .)
		},
		parallel = {
			tbl <-
				tibble(test_num = 1:num) %>%
				mutate(
					vars = map(test_num, ~ c(exposures, covariates[.x - 1 + is.null(exposures)]))
				) %>%
				tidyr::expand_grid(outcomes = outcomes, .)
		}
	)

	# Expand formulas
	tbl <-
		tbl %>%
		mutate(formulas = purrr::map_chr(vars, ~paste(unlist(.x), collapse = " + "))) %>%
		mutate(formulas = paste(outcomes, formulas, sep = " ~ ")) %>%
		mutate(formulas = map(formulas, ~formula(.x)))

	# Return
	tbl

}

#' @description Create strata from data in `add_hypothesis()`
#' @noRd
create_strata <- function(data = NULL, strata) {

	# Check to see if data is available
	if (is.null(data)) {
		stop("Strata cannot be created if a data set is not available.")
	}

	# Expand strata
	level <- unique(dplyr::pull(data, strata))

	# Return
	level

}

#' @description Update status flags
#' @noRd
flag_status <- function(project, name, row, strata, type) {

	# Open status flags
	status <- project$status[[row]]

	# Create important parameters
	has_split <- !is.null(strata)
	if (is.null(strata)) {has_strata <- NA} else {has_strata <- strata}
	has_type <- type

	# Add hypothesis status
	status[[name]] <- tibble::tibble(
		run = FALSE,
		split = has_split,
		strata = has_strata,
		type = has_type
	)

	# Add status back in
	project$status[[row]] <- status

	# Return
	project

}

#' @description Identify type of test that will be used
#' @noRd
type_of_test <- function(test) {

	# Check if modeling type
	if ("model_spec" %in% class(test)) {
		type <- "model_spec"
	}

	# Check if hypothesis test type
	if ("character" %in% class(test)) {
		# Check to see if viable function
		fn <- get(test)
		if (is.function(fn)) {
			type <- "htest"
		}
		else {
			stop("The `test` is not a function that can be passed on.")
		}
	}
	# Validation of type
	else if ("character" %in% class(test)) {
		stop("The `test` is not a character string.", call. = FALSE)
	}

	# Return
	type
}

