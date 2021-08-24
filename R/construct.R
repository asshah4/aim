#' Construct Models
#'
#' This function allows for delayed building of multiple hypothesis. It uses the
#' `hypothesis` objects with the corresponding __test__ arguments against the
#' prescribed __data__. Which hypotheses to run can be specified, which will
#' forcibly re-run these, otherwise the default behavior is to only run models
#' that have not yet been fitted.
#'
#' @return Returns a `study` object that has be fitted and tidied.
#'
#' @inheritParams draw_hypothesis
#'
#' @param which_ones Vector of which hypothesis should be built. Defaults to
#'   building all hypotheses that have not been run yet. If given a name, will
#'   forcibly re-run the analysis.
#'
#' @family studies
#' @export
construct_models <- function(study, which_ones = NULL, ...) {

	validate_class(study, "study")

	# Model map
	m <- study$model_map

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
			fits <- fit_parsnip_models(.formula = formulae, .test = test, .data = data)
			tidied <- tidy_tests(.fits = fits)

			# Return to original study object
			m$fit[m$name == name] <- fits
			m$tidy[m$name == name] <- tidied

			# Update stage
			study <-
				update_study(study, name = name, stage = "built", run = TRUE)
		}
	} else {
		message("All tests are already built. To force build, set `which_ones` to desired hypotheses.")
	}

	# Replace
	study$model_map <- m

	# Return
	study

}

#' Construct Path Maps
#' @export
construct_maps <- function(...) {

}

#' Construct Directed Acyclic Graphs
#'
#' This function converts a hypothesis into a `dagitty` object (or
#' `tidy_dagitty` if requested). This can subsequently be passed onto the
#' [ggdag::ggdag()] function for additional plotting.
#'
#' @return `dagitty` or `tidy_dagitty` object
#' @inheritParams draw_hypothesis
#' @param tidy Defaults to FALSE, thus returning a `dagitty` object. If TRUE, then will return a `tidy_dagitty` object.
#' @importFrom rlang !!!
#' @export
construct_dag <- function(study, name, tidy = FALSE) {

	p <- study$path_map
	f <- p$relationship

	exp <- unique(p$exposure[p$name == name])
	out <- unique(p$outcome[p$name == name])

	if (tidy) {
		rlang::exec(ggdag::dagify, !!!f, exposure = exp, outcome = out) %>%
		ggdag::tidy_dagitty(.)
	} else {
		rlang::exec(ggdag::dagify, !!!f, exposure = exp, outcome = out)
	}

}
