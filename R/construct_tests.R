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
#' @return Invisibly returns a `model_map` object that has the hypotheses mapped to
#'   it, including fits and paths
#'
#' @param model_map A `model_map` object that contains `hypothesis` objects
#'
#' @param which_ones Vector of which hypothesis should be constructed. It
#'   defaults to NULL, which constructs all hypotheses that have not yet been
#'   processed. If a vector of names is given, will forcibly re-analyze them.
#'
#' @param ... For extensibility
#'
#' @family studies
#' @export
construct_tests <- function(model_map, which_ones = NULL, ...) {

	validate_class(model_map, "model_map")
	validate_stage(model_map, "hypothesis")

	# Model map, with appropriate types for fits and tidy
	m <- model_map
	m$fit <- as.list(m$fit)
	m$tidy <- as.list(m$tidy)

	# Select out models that have not yet been run
	# If specified hypothesis are named, force them to be re-run
	if (is.null(which_ones)) {
		x <-
			attributes(model_map)$status_table[c("name", "run")] %>%
			subset(., run == FALSE)
	} else {
		x <-
			attributes(model_map)$status_table[c("name", "run")] %>%
			subset(., name %in% which_ones)
	}

	if (nrow(x) > 0) {
		for (i in 1:nrow(x)) {
			name <- x$name[i]
			y <- m[m$name == name, ]

			# Retrieve information
			test <- fetch_test(model_map, name)
			data <- fetch_data(model_map, name)
			strata <- fetch_strata(model_map, name)

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
			m <- .revise_status(m, name, run = TRUE)
		}
	} else {
		message("All tests are already built. To force build, set `which_ones` to desired hypotheses.")
	}

	# Replace model data
	model_map <- m

	# Return
	invisible(model_map)

}
