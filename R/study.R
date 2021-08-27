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
			~name, ~outcomes, ~exposures, ~number, ~formulae, ~fit, ~tidy
		),
		path_map = tibble::tribble(
			~name, ~outcomes, ~exposures, ~from, ~direction, ~to, ~formulae, ~type, ~related
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

	# Variable table
	attr(study, "var_table") <- tibble::tribble(
		~name, ~outcomes, ~exposures, ~confounders, ~fixed
	)

	# Return
	structure(
		study,
		class = c("study", class(study))
	)

}


# Generics ----

#' Print a Study
#' @param x A `study` object
#' @param ... further arguments passed to or from other methods
#' @export
print.study <- function(x, ...) {

	# Retrieve variables
	s <- deparse(substitute(x))
	m <- x$model_map
	h <- unique(m$name)
	p <- x$path_map
	n <- length(unique(as.character(p$formulae)))

	# Printing
	cat(glue::glue(
		"
		# A study with {length(h)} hypothesis and {n} unique paths
		# \n
		"
	))
	print(m)

}
