#' Mapping Many Hypotheses Together
#'
#' @description
#'
#' `r lifecycle::badge('stable')`
#'
#' Calling `create_study()` initializes a list object that stores `hypothesis` objects,
#' which are used to explore a research project. It allows for delayed building
#' of models, and is used to help study the relationship between variables.
#'
#' A `study` object is a essentially a modified list containing two
#' complementary table structures, which are interrelated via the `hypothesis`
#' objects, and several attributes that allow for information storage about the
#' object itself.
#'
#' @details
#'
#' The two primary data structures for a `study` object are the __model_map__ table and the __path_map__ table, which are interrelated and allow for reformulation of hypotheses and graphical understanding of relationships. Additional attributes are stored in the study to help simplify analyses behind the scenes.
#'
#' ## Model Maps
#'
#' This is the primarily visualized system and creates the base for the `study` object.
#'
#' ## Path Maps
#'
#' Paths are proposed variable relationships when adding hypotheses. The relationship directions are theoretical, as the user proposes a hypothesis with the primary exposure and covariates being considered as potential confounders. By adding additional hypotheses, the amount of paths that are analyzed are increased.
#'
#' When mapping paths, they are analyzed by which paths can coexist. This is judged by:
#'
#'   * must come from the same __data__
#'
#'   * may use the same __test__
#'
#' @param ... For extensibility
#'
#' @return A `study` object
#' @family studies
#' @export
create_study <- function(...) {

	# Base structure is that of a list of two tibbles
	study <- list(
		model_map = tibble::tribble(
			~name, ~outcomes, ~exposures, ~level, ~number, ~formulae, ~fit, ~tidy
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

	# Distribution table
	attr(study, "dist_table") <- tibble::tribble(
		~term, ~distribution, ~data_name
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
		"# A study with {length(h)} ",
		"{if (length(h) == 1) 'hypothesis' else 'hypotheses'}",
		" and {n} unique path",
		"{if (n > 1) 's' else ''}",
	))
	cat("\n")
	cat("#\n")
	print(m)

}

#' Study Summary
#' @param object a `study` object
#' @param ... further arguments passed to or from other methods
summary.study <- function(object, ...) {

	# Retrieve variables
	m <- object$model_map
	p <- object$path_map
	d <- attributes(object)$data_list
	d$rows <- nrow(d$data[[1]])
	d$columns <- length(d$data[[1]])
	d <- subset(d, select = -data)
	h <-
		attributes(object)$test_table %>%
		dplyr::inner_join(., attributes(object)$data_table, by = "name") %>%
		dplyr::inner_join(., attributes(object)$status_table, by = "name") %>%
		dplyr::select("name", "type", "combination", "data_name", "strata", "run")


	# Metadata
	study_name <- deparse(substitute(object))
	cat(glue::glue(
		"
		------------------{paste0(rep('-', nchar(study_name)), collapse = '')}
		Summary of Study: {study_name}
		------------------{paste0(rep('-', nchar(study_name)), collapse = '')}


		"
	))

	# Hypotheses
	cat("Hypothesis:\n\n")
	glue::glue("{knitr::kable(h, format = 'simple')}") %>% print()
	cat("\n")

	# Data
	cat("Data:\n\n")
	glue::glue("{knitr::kable(d, format = 'simple')}") %>% print()

}
