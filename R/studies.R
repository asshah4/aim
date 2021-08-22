#' Initialize a Framework for studiesing Hypotheses
#'
#' @family studies
#' @export
studies <- function(...) {

	# Base structure is that of a tibble
	studies <- tibble::tribble(
		~name, ~number, ~outcome, ~exposure, ~formulae, ~fit, ~tidy
	)

	# Need to know how the data should be tested
	attr(studies, "data_table") <- tibble::tribble(
		~name, ~data_name, ~strata
	)

	# Storage of data should be simple
	attr(studies, "data_list") <- tibble::tribble(
		~data_name, ~data
	)

	# Identify the tests
	attr(studies, "test_table") <- tibble::tribble(
		~name, ~call, ~test, ~test_opts, ~combination, ~type
	)

	# Recording of status updates
	attr(studies, "status_table") <- tibble::tribble(
		~name, ~run, ~error, ~stage
	)

	attr(studies, "var_table") <- tibble::tribble(
		~name, ~combination, ~outcomes, ~exposures, ~fixed, ~covariates, ~confounders
	)

	# Return
	studies <-
		structure(studies, class = c("studies", class(studies)))

}

#' Add a Hypothesis to the studies
#'
#' Takes a `hypothesis` object and adds it to a `studies`, allowing multiple
#' potential hypotheses to be put together for eventual analysis and comparison.
#'
#' @return A `studies` object that has had a `hypothesis` added
#'
#' @param studies Object of class `studies`
#'
#' @param hypothesis Object of class `hypothesis` (which may or may not include
#'   data already added)
#'
#' @param name Name of the `hypothesis` object, which defaults to the name of
#'   the `hypothesis` object itself
#'
#' @family studies
#' @export
add_hypothesis <- function(studies,
													 hypothesis,
													 name = deparse(substitute(hypothesis)),
													 ...) {

	validate_class(studies, "studies")

	# Pipe of adding components to studies
	studies <-
		studies %>%
		.link_hypothesis(hypothesis, name) %>%
		.link_test(hypothesis, name) %>%
		.link_data(hypothesis, name) %>%
		.link_status(hypothesis, name) %>%
		.link_vars(hypothesis, name)

	# Update stage/status
	studies <- update_studies(studies, name, stage = "hypothesis")

	# Return
	studies
}

#' Run the studies by Fitting Models and Tests
#'
#' This function allows for delayed building of multiple hypothesis. It uses the
#' `hypothesis` objects with the corresponding __test__ arguments against the
#' prescribed __data__. Which hypotheses to run can be specified, which will
#' forcibly re-run these, otherwise the default behavior is to only run models
#' that have not yet been fitted.
#'
#' @return Returns a `studies` object that has be fitted and tidied.
#'
#' @inheritParams add_hypothesis
#'
#' @param which_ones Vector of which hypothesis should be built. Defaults to
#'   building all hypotheses that have not been run yet. If given a name, will
#'   forcibly re-run the analysis.
#'
#' @family studies
#' @export
build_studies <- function(studies, which_ones = NULL, ...) {

	validate_class(studies, "studies")

	# Select out models that have not yet been run
	# If specified hypothesis are named, force them to be re-run
	if (is.null(which_ones)) {
		x <-
			attributes(studies)$status_table[c("name", "run")] %>%
			subset(., run == FALSE)
	} else {
		x <-
			attributes(studies)$status_table[c("name", "run")] %>%
			subset(., name %in% which_ones)
	}

	if (nrow(x) > 0) {
		for (i in 1:nrow(x)) {
			name <- x$name[i]

			# Retrieve information
			test <- fetch_test(studies, name)
			data <- fetch_data(studies, name)
			formulae <- fetch_formulae(studies, name)

			# Apply fitting and tidying functions
			fits <- fit_models(.formula = formulae, .test = test, .data = data)
			tidied <- tidy_tests(.fits = fits)

			# Return to original studies object
			studies$fit[studies$name == name] <- fits
			studies$tidy[studies$name == name] <- tidied

			# Update stage
			studies <-
				update_studies(studies, name = name, stage = "built", run = TRUE)
		}
	} else {
		message("All tests are already built. To force build, set `which_ones` to desired hypothesis.")
	}

	# Return
	studies
}

