#' Extract models from a study
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' This function serves as a non-pipeable extraction function to obtain
#' aggregate components from a `study` object. Its designed to have a simple
#' call interface to allow for retrieval of model objects to be formatted and
#' displayed.
#'
#' @return A table that has either raw or tidy lists of model fits
#'
#' @param study A `study` object that contains `hypothesis` objects that have
#'   been run
#'
#' @param which_ones Vector of names of hypotheses that should be extracted and
#'   distilled. Will only be applied to models that have been fit. Defaults to
#'   NULL, for which all tests will be returned.
#'
#' @param tidy Logical value for if the returned models should be collected into
#'   a tidy table or as a list of raw model/test fits. Defaults to TRUE.
#'
#' @param ... For extensibility
#' @export
extract_models <- function(study, which_ones = NULL, tidy = TRUE, ...) {

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
			.[c("name", "outcomes", "exposures", "level", "number", "fit")]

	} else if (nrow(x) > 0 & tidy) {

		study$model_map %>%
			.[.$name %in% x$name, ] %>%
			.[c("name", "outcomes", "exposures", "level", "number", "tidy")] %>%
			tidyr::unnest(tidy)

	} else {
		message("The models `", paste(which_ones, sep = ", "), "` have not been fit.")
	}

}

#' Extract a `tidy_dagitty` object
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' This function converts a hypothesis into a `dagitty` object (or
#' `tidy_dagitty` if requested). This can subsequently be passed onto the
#' [ggdag::ggdag()] function for additional plotting.
#'
#' @return `dagitty` or `tidy_dagitty` object
#'
#' @param study A `study` object
#'
#' @param name The name of a hypothesis added to the study
#'
#' @param tidy Defaults to FALSE, thus returning a `dagitty` object. If TRUE,
#'   then will return a `tidy_dagitty` object.
#'
#' @importFrom rlang !!!
#' @export
extract_dagitty <- function(study, name, tidy = FALSE) {

	p <- study$path_map
	f <- p$formulae

	exp <- unique(p$exposures[p$name == name])
	out <- unique(p$outcomes[p$name == name])

	if (tidy) {
		rlang::exec(ggdag::dagify, !!!f, exposure = exp, outcome = out) %>%
		ggdag::tidy_dagitty(.)
	} else {
		rlang::exec(ggdag::dagify, !!!f, exposure = exp, outcome = out)
	}

}
