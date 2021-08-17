#' Initialize a Framework for Studying Hypotheses
#'
#' @family studies
#' @export
study <- function(...) {

	# Base structure is that of a tibble
	study <- tibble::tribble(
		~name, ~number, ~outcome, ~exposure, ~formulae, ~fit, ~tidy
	)

	# Need to know how the data should be tested
	attr(study, "data_table") <- tibble::tribble(
		~name, ~data_name, ~strata
	)

	# Storage of data should be simple
	attr(study, "data_list") <- tibble::tribble(
		~data_name, ~data
	)

	# Identify the tests
	attr(study, "test_table") <- tibble::tribble(
		~name, ~call, ~test, ~test_opts, ~combination, ~type
	)

	# Recording of status updates
	attr(study, "status_table") <- tibble::tribble(
		~name, ~run, ~error, ~stage
	)

	attr(study, "var_table") <- tibble::tribble(
		~name, ~combination, ~outcomes, ~exposures, ~fixed, ~covariates, ~confounders
	)

	# Return
	study <-
		structure(study, class = c("study", class(study)))

}

#' Add a Hypothesis to the Study
#'
#' Takes a `hypothesis` object and adds it to a `study`, allowing multiple
#' potential hypotheses to be put together for eventual analysis and comparison.
#'
#' @return A `study` object that has had a `hypothesis` added
#'
#' @param study Object of class `study`
#'
#' @param hypothesis Object of class `hypothesis` (which may or may not include
#'   data already added)
#'
#' @param name Name of the `hypothesis` object, which defaults to the name of
#'   the `hypothesis` object itself
#'
#' @family studies
#' @export
add_hypothesis <- function(study,
													 hypothesis,
													 name = deparse(substitute(hypothesis)),
													 ...) {

	validate_class(study, "study")

	# Pipe of adding components to study
	study <-
		study %>%
		.link_hypothesis(hypothesis, name) %>%
		.link_test(hypothesis, name) %>%
		.link_data(hypothesis, name) %>%
		.link_status(hypothesis, name) %>%
		.link_vars(hypothesis, name)

	# Update stage/status
	study <- update_study(study, name, stage = "hypothesis")

	# Return
	study
}

#' Run the Study by Fitting Models and Tests
#'
#' This function allows for delayed building of multiple hypothesis. It uses the
#' `hypothesis` objects with the corresponding __test__ arguments against the
#' prescribed __data__. Which hypotheses to run can be specified, which will
#' forcibly re-run these, otherwise the default behavior is to only run models
#' that have not yet been fitted.
#'
#' @return Returns a `study` object that has be fitted and tidied.
#'
#' @inheritParams add_hypothesis
#'
#' @param which_ones Vector of which hypothesis should be built. Defaults to
#'   building all hypotheses that have not been run yet. If given a name, will
#'   forcibly re-run the analysis.
#'
#' @family studies
#' @export
build_study <- function(study, which_ones = NULL, ...) {

	validate_class(study, "study")

	# Select out models that have not yet been run
	# If specified hypothesis are named, force them to be re-run
	if (is.null(which_ones)) {
		x <-
			attributes(study)$status_table[c("name", "run")] %>%
			subset(., run == FALSE)
	} else {
		x <-
			attributes(study)$status_table[c("name", "run")] %>%
			subset(., name %in% which_ones)
	}

	if (nrow(x) > 0) {
		for (i in 1:nrow(x)) {
			name <- x$name[i]

			# Retrieve information
			test <- fetch_test(study, name)
			data <- fetch_data(study, name)
			formulae <- fetch_formulae(study, name)

			# Apply fitting and tidying functions
			fits <- fit_models(.formula = formulae, .test = test, .data = data)
			tidied <- tidy_tests(.fits = fits)

			# Return to original study object
			study$fit[study$name == name] <- fits
			study$tidy[study$name == name] <- tidied

			# Update stage
			study <-
				update_study(study, name = name, stage = "built", run = TRUE)
		}
	} else {
		message("All tests are already built. To force build, set `which_ones` to desired hypothesis.")
	}

	# Return
	study
}

