#' Initialize a Hypothesis Framework
#'
#' @family frameworks
#' @export
framework <- function(...) {
	# Base structure is that of a tibble
	framework <- tibble::tribble(
		~name, ~number, ~outcome, ~exposure, ~formulae, ~fit, ~tidy
	)

	attr(framework, "data_table") <- tibble::tribble(
		~name, ~data_name, ~strata
	)

	attr(framework, "data_list") <- tibble::tribble(
		~data_name, ~data
	)

	attr(framework, "test_table") <- tibble::tribble(
		~name, ~call, ~test, ~test_opts, ~combination, ~type
	)

	attr(framework, "status_table") <- tibble::tribble(
		~name, ~run, ~error, ~stage
	)

	# Return
	framework <-
		structure(framework, class = c("framework", class(framework)))
}

#' Add a Hypothesis to the Framework
#'
#' Takes a `hypothesis` object and adds it to a `framework`, allowing multiple
#' potential hypotheses to be put together for eventual analysis and comparison.
#'
#' @return A `framework` object that has had a `hypothesis` added
#'
#' @param framework Object of class `framework`
#'
#' @param hypothesis Object of class `hypothesis` (which may or may not include
#'   data already added)
#'
#' @param name Name of the `hypothesis` object, which defaults to the name of
#'   the `hypothesis` object itself
#'
#' @family frameworks
#' @export
add_hypothesis <- function(framework,
													 hypothesis,
													 name = deparse(substitute(hypothesis)),
													 ...) {

	validate_class(framework, "framework")

	# Pipe of adding components to framework
	framework <-
		framework %>%
		.link_hypothesis(hypothesis, name) %>%
		.link_test(hypothesis, name) %>%
		.link_data(hypothesis, name) %>%
		.link_status(hypothesis, name)

	# Update stage/status
	framework <- update_status(framework, name, stage = "hypothesis")

	# Return
	framework
}

#' Build Up the Framework by Fitting Models and Tests
#'
#' This function allows for delayed building of multiple hypothesis. It uses the
#' `hypothesis` objects with the corresponding __test__ arguments against the
#' prescribed __data__. Which hypotheses to run can be specified, which will
#' forcibly re-run these, otherwise the default behavior is to only run models
#' that have not yet been fitted.
#'
#' @return Returns a `framework` object that has be fitted and tidied.
#'
#' @inheritParams add_hypothesis
#'
#' @param which_ones Vector of which hypothesis should be built. Defaults to
#'   building all hypotheses that have not been run yet. If given a name, will
#'   forcibly re-run the analysis.
#'
#' @family frameworks
#' @export
build_frames <- function(framework, which_ones = NULL, ...) {

	validate_class(framework, "framework")

	# Select out models that have not yet been run
	# If specified hypothesis are named, force them to be re-run
	if (is.null(which_ones)) {
		x <-
			attributes(framework)$status_table[c("name", "run")] %>%
			subset(., run == FALSE)
	} else {
		x <-
			attributes(framework)$status_table[c("name", "run")] %>%
			subset(., name %in% which_ones)
	}

	if (nrow(x) > 0) {
		for (i in 1:nrow(x)) {
			name <- x$name[i]

			# Retrieve information
			test <- get_test(framework, name)
			data <- get_data(framework, name)
			formulae <- get_formulae(framework, name)

			# Apply fitting and tidying functions
			fits <- fit_models(.formula = formulae, .test = test, .data = data)
			tidied <- tidy_tests(.fits = fits)

			# Return to original framework object
			framework$fit[framework$name == name] <- fits
			framework$tidy[framework$name == name] <- tidied

			# Update stage
			framework <-
				update_status(framework, name = name, stage = "built", run = TRUE)
		}
	} else {
		message("All tests are already built. To force build, set `which_ones` to desired hypothesis.")
	}

	# Return
	framework
}

