#' Draw Hypothesis onto Map
#'
#' @description
#' `r lifecycle::badge('experimental')`
#' Takes a `hypothesis` object and adds it to a `study`, allowing multiple
#' potential hypotheses to be put together for eventual analysis and comparison.
#' This involves the deconstructing of the hypothesis into its individual parts
#' and recomposition of the theorized formula.
#'
#' Simultaneously, takes the `hypothesis` objects that were added to the `study`
#' and decomposes them into specific **paths** that are used to help define
#' relationships between variables. These paths are stored in the `study` itself
#' in the form of a `data.frame`. This is currently experimental in that the
#' directionality, relationships, and patterns are intended to be used to help
#' identify certain variables for future modeling, but the implementation is not
#' yet complete.
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
#' @param ... For extensibility
#'
#' @family studies
#' @export
draw_hypothesis <- function(study,
														hypothesis,
														name = deparse(substitute(hypothesis)),
														...) {

	validate_class(study, "study")

	# The hypothesis should be broken down to be incorporated into the study
	study <-
		study %>%
		add_study_formula(hypothesis, name) %>%
		add_study_test(hypothesis, name) %>%
		add_study_data(hypothesis, name) %>%
		add_study_status(hypothesis,
										 name,
										 run = FALSE,
										 path = FALSE,
										 origin = NA)

	# Paths can subsequently be added
	study <-
		study %>%
		add_study_path(name) %>%
		update_study_status(name, path = FALSE)

	# Return
	study
}
