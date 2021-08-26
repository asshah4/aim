# Core Functions ----

#' Mapping Many Hypotheses Together
#'
#' Calling `study()` initializes a list object that stores `hypothesis` objects,
#' which are used to explore a research project. It allows for delayed building
#' of models, and is used to help study the relationship between variables.
#'
#' A `study` object is a essentially a modified list containing two
#' complementary table structures, which are interrelated via the `hypothesis`
#' objects, and several attributes that allow for information storage about the
#' object itself.
#'
#' @param ... For extensibility
#'
#' @return A `study` object
#' @family studies
#' @export
study <- function(...) {

	# Base structure is that of a list of two tibbles
	study <- list(
		model_map = tibble::tribble(
			~name, ~outcome, ~exposure, ~number, ~formulae, ~fit, ~tidy
		),
		path_map = tibble::tribble(
			~name, ~outcome, ~exposure, ~relationship, ~term, ~direction, ~to, ~type
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

	# Identify the tests, with origin of hypothesis being related for reformulation
	attr(study, "test_table") <- tibble::tribble(
		~name, ~call, ~test, ~test_opts, ~combination, ~type
	)

	# Recording of status updates
	attr(study, "status_table") <- tibble::tribble(
		~name, ~run, ~path, ~error, ~origin
	)

	# Return
	structure(
		study,
		class = c("study", class(study))
	)

}

#' Draw Hypothesis onto Map
#'
#' Takes a `hypothesis` object and adds it to a `study`, allowing multiple
#' potential hypotheses to be put together for eventual analysis and comparison.
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

	# Return
	study
}

# Generics ----

#' Print a Study
#' @param x A `study` object
#' @inheritParams base::print
#' @export
print.study <- function(x, ...) {

	# Retrieve variables
	s <- deparse(substitute(x))
	m <- x$model_map
	h <- unique(m$name)
	p <- x$path_map
	n <- length(unique(as.character(p$relationship)))

	# Printing
	cat(glue::glue(
		"
		# A study with {length(h)} hypothesis and {n} unique paths
		# \n
		"
	))
	print(m)

}
