#' Construct Map
#'
#' @description
#' `r lifecycle::badge('experimental')`
#' This function allows for delayed building of multiple hypothesis. It uses the
#' `hypothesis` objects with the corresponding __test__ arguments against the
#' prescribed __data__. Which hypotheses to run can be specified, which will
#' forcibly re-run these, otherwise the default behavior is to only run models
#' that have not yet been fitted.
#'
#' Simultaneously, Takes the `hypothesis` objects that were added to the `study`
#' and decomposes them into specific **paths** that are used to help define
#' relationships between variables. These paths are stored in the `study` itself
#' in the form of a `data.frame`. This is currently experimental in that the
#' directionality, relationships, and patterns are intended to be used to help
#' identify certain variables for future modeling, but the implementation is not
#' yet complete.
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

			# Update status
			study <-
				update_study_status(study, name, run = TRUE)
		}
	} else {
		message("All tests are already built. To force build, set `which_ones` to desired hypotheses.")
	}

	# Replace model data
	study$model_map <- m

	# Now create paths
	if (nrow(x) > 0) {
		for (i in 1:nrow(x)) {
			name <- x$name[i]

			# List of appropriate models
			models <-
				study$model_map %>%
				.[.$name == name, ] %>%
				.[.$number == max(.$number), ] %>%
				unique()

			for (j in 1:nrow(models)) {

				# Terms
				f <- models[j, ]$formulae[[1]]
				exp <- models[j, ]$exposure[[1]]
				out <- as.character(f[[2]])

				# Path formulas
				paths <- expand_paths(f)

				# Create paths and update original data
				for (k in 1:length(paths)) {
					study$path_map <-
						study$path_map %>%
						tibble::add_row(
							name = name,
							outcome = out,
							exposure = exp,
							formulae = paths[k],
							from = as.character(paths[[k]][[3]]),
							direction = "->",
							to = as.character(paths[[k]][[2]]),
							type = NA,
							related = NA
						)
				}
			}

			# Update status
			study <-
				update_study_status(study, name, path = TRUE)
		}
	}

	# Return
	invisible(study)

}
