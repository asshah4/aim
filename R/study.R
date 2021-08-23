#' Mapping Many Hypotheses Together
#'
#' @export
study <- function(...) {

	# Base structure is that of a list of two tibbles
	study <- list(
		model_map = tibble::tribble(
			~name, ~outcome, ~exposure, ~number, ~formulae, ~fit, ~tidy
		),
		path_map = tibble::tribble(
			~name, ~outcome, ~exposure, ~from, ~direction, ~to, ~position
		)
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
		~name, ~run, ~error, ~stage, ~path
	)

	# Return
	structure(
		study,
		class = c("study", class(study))
	)

}

#' Draw Hypothesis onto Map
#' @export
draw_hypothesis <- function(study,
														hypothesis,
														name = deparse(substitute(hypothesis)),
														...) {

	validate_class(study, "study")

	# The hypothesis should be broken down to be incorporated into the study
	study <-
		study %>%
		modify_study_test(hypothesis, name) %>%
		modify_study_data(hypothesis, name) %>%
		modify_study_formula(hypothesis, name) %>%
		modify_study_status(hypothesis, name, run = FALSE, stage = "hypothesis")



	# Return
	study
}
