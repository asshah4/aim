#' Mapping Many Hypotheses Together
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' Calling `create_map()` initializes a list object that stores `hypothesis` objects,
#' which are used to explore a research project. It allows for delayed building
#' of models, and is used to help study the relationship between variables.
#'
#' A `model_map` object is a essentially a modified list containing two
#' complementary table structures, which are interrelated via the `hypothesis`
#' objects, and several attributes that allow for information storage about the
#' object itself.
#'
#' @param ... For extensibility
#'
#' @return A `model_map` object
#' @family maps
#' @importFrom tibble tribble
#' @importFrom dplyr mutate across
#' @export
create_map <- function(...) {

	# Base structure is that of a list of two tibbles
	model_map <-
		tribble(
			~name, ~outcome, ~exposure, ~level, ~number, ~formulae, ~fit, ~tidy
		) %>%
		mutate(
			across(c(name, outcome, exposure, level), as.character),
			across(number, as.integer),
			across(c(formulae, fit, tidy), as.list)
		)

	# Need to know how the data should be tested
	attr(model_map, "data_table") <-
		tribble(~name, ~data_name, ~strata) %>%
		mutate(
			across(c(name, data_name, strata), as.character)
		)

	# Storage of data should be simple
	attr(model_map, "data_list") <-
		tribble(~data_name, ~data) %>%
		mutate(
			across(data_name, as.character),
			across(data, as.list)
		)

	# Identify the tests, with origin of hypothesis being related for reformulation
	attr(model_map, "test_table") <-
		tribble(~name, ~call, ~test, ~test_opts, ~combination, ~type) %>%
		mutate(
			across(c(name, combination, type), as.character),
			across(c(call, test, test_opts), as.list)
		)

	# Recording of status updates
	attr(model_map, "status_table") <-
		tribble(~name, ~run, ~error, ~origin) %>%
		mutate(
			across(c(name, origin), as.character),
			across(c(run, error), as.logical)
		)

	# Variable table
	attr(model_map, "relation_table") <-
		tribble(~name, ~outcome, ~exposure, ~confounder, ~fixed) %>%
		mutate(
			across(c(name, outcome, exposure, confounder, fixed), as.character)
		)

	# Distribution table
	attr(model_map, "variable_table") <-
		tribble(~term, ~distribution, ~data_name) %>%
		mutate(
			across(c(term, distribution, data_name), as.character)
		)

	# Return
	structure(
		model_map,
		class = c("model_map", class(model_map))
	)

}

# Helper Functions ----

#' @rdname modify_map
.revise_status <- function(model_map, name, ...) {

	# Match call
	mc <- match.call(expand.dots = TRUE)
	changes <- list(...)

	# Get named model_map status
	x <- attr(model_map, "status_table")
	y <- x[x$name == name, ]

	# Names
	new_names <- names(changes)
	old_names <- names(y)

	# Update status
	for (i in new_names) {
		y[[i]] <- changes[[i]]
	}

	# Replace in model_map
	x[x$name == name, ] <- y
	attr(model_map, "status_table") <- x

	# Return invisibly
	invisible(model_map)

}

# Generics ----

#' Print a Model Map
#' @param x A `model_map` object
#' @param ... further arguments passed to or from other methods
#' @export
print.model_map <- function(x, ...) {

	# Retrieve variables
	s <- deparse(substitute(x))
	m <- x
	h <- unique(m$name)

	# Printing
	cat(glue::glue(
		"# A map with {length(h)} ",
		"{if (length(h) == 1) 'hypothesis' else 'hypotheses'}",
	))
	cat("\n")
	cat("#\n")
	print(tibble::as_tibble(m))

}

#' model_map Summary
#' @param object a `model_map` object
#' @param ... further arguments passed to or from other methods
summary.model_map <- function(object, ...) {

	# Retrieve variables
	m <- object
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
	model_name <- deparse(substitute(object))
	cat(glue::glue(
		"
		----------------------{paste0(rep('-', nchar(model_name)), collapse = '')}
		Summary of Model Map: {model_name}
		----------------------{paste0(rep('-', nchar(model_name)), collapse = '')}


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
