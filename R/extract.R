#' Extract a Model Hypothesis from a Study
#'
#' @description
#'
#' `r lifecycle::badge('stable')`
#'
#' Extracts the model components of a hypothesis that has been ran. Everything
#' is returned as a table, with a column of either tidy or raw results.
#'
#' @return A table of model/test fits, with either raw or tidied results
#'
#' @param study A `study` object that contains `hypothesis` objects that have
#'   been run
#'
#' @param which_ones Vector of names of hypotheses that should be extracted.
#'   Will only return models that have been fit. Defaults to NULL, for which all
#'   tests will be returned.
#'
#' @param tidy Logical value for if the returned models should be collected into
#'   a tidy table or as a list of raw model/test fits. Defaults to TRUE.
#'
#' @param ... For extensibility
#' @export
extract <- function(study, which_ones = NULL, tidy = TRUE, ...) {

	# Validate
	validate_class(study, "study")
	validate_stage(study, "run")

	# Select out models that have not yet been run
	# If specified hypothesis are named, force them to be re-run
	if (is.null(which_ones)) {
		x <-
			attributes(study)$status_table[c("name", "run")] %>%
			subset(., run == TRUE)
	} else {
		x <-
			attributes(study)$status_table[c("name", "run")] %>%
			subset(., name %in% which_ones) %>%
			subset(., run == TRUE)
	}

	# Return models
	if (nrow(x) > 0 & !tidy) {

		study$model_map %>%
			.[.$name %in% x$name, ] %>%
			.[c("name", "outcomes", "exposures", "number", "fit")]

	} else if (nrow(x) > 0 & tidy) {

		study$model_map %>%
			.[.$name %in% x$name, ] %>%
			.[c("name", "outcomes", "exposures", "number", "tidy")] %>%
			tidyr::unnest(tidy)

	} else {
		message("The models `", paste(which_ones, sep = ", "), "` have not been fit.")
	}

}
