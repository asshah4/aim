#' Construct Map
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' This function allows for delayed building of multiple hypothesis. It uses the
#' `hypothesis` objects with the corresponding __test__ arguments against the
#' prescribed __data__. Which hypotheses to run can be specified, which will
#' forcibly re-run these, otherwise the default behavior is to only run models
#' that have not yet been fitted.
#'
#' @return Invisibly returns a `study` object that has the hypotheses mapped to
#'   it, including fits and paths
#'
#' @param study A `study` object that contains `hypothesis` objects
#'
#' @param which_ones Vector of which hypothesis should be constructed. It
#'   defaults to NULL, which constructs all hypotheses that have not yet been
#'   processed. If a vector of names is given, will forcibly re-analyze them.
#'
#' @param ... For extensibility
#'
#' @family studies
#' @export
construct_map <- function(study, which_ones = NULL, ...) {

	validate_class(study, "study")
	validate_stage(study, "hypothesis")

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
			y <- m[m$name == name, ]

			# Retrieve information
			test <- fetch_test(study, name)
			data <- fetch_data(study, name)
			strata <- fetch_strata(study, name)

			# Apply fitting and tidying functions
			if (is.na(strata)) {
				y <-
					y %>%
					mutate(fit = purrr::map(formulae, ~ possible_parsnip_fit(test, .x, data)))
			} else if (!is.na(strata)) {
				y <-
					y %>%
					mutate(fit = purrr::map2(formulae, level, ~ possible_parsnip_fit(test, .x, data[data[[strata]] == .y, ])))
			}

			# Tidy all fits
			y <-
				y %>%
				mutate(tidy = purrr::map(fit, ~ broom::tidy(.x, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE)))

			# Return to original study object
			m[m$name == name, ] <- y

			# Update status
			study <-
				update_study_status(study, name, run = TRUE)
		}
	} else {
		message("All tests are already built. To force build, set `which_ones` to desired hypotheses.")
	}

	# Replace model data
	study$model_map <- m

	# Return
	invisible(study)

}
