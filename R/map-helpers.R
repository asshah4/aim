# Retrieve Functions ----

#' Retrieve, Modify, and Destroy Components From `study` Object
#'
#' @description
#'
#' These functions are essentially helper functions to simplify the manipulation
#' of `study` objects. They may retrieve or modify or destroy components of
#' the `study` object as described.
#'
#' __Retrieval functions__:
#'
#' * `fetch_data()` retrieves the data set associated with a specified hypothesis
#' and data name
#'
#' * `fetch_test()` retrieves the test associated with a hypothesis
#'
#' * `fetch_formulae()` retrieves the list of formulas associated with a
#' hypothesis
#'
#' * `fetch_combination()` retrieves how a hypothesis was combined to generate its
#' formula list
#'
#' * `fetch_names()` retrieves the hypothesis names that match a certain argument, such as `combination = "parallel"` or `data_name = "mtcars"`
#'
#' __Update functions__:
#'
#' * `update_study()` modifies the components of a study, usually used
#' internally to help update the stage/status of the specified object
#'
#' @return The `fetch_*()` functions return the named object from the `study`.
#'   The `update_*()` functions return the entire study after modification.
#'
#' @param x `study` object
#'
#' @param name Name of `hypothesis` object to pull components from
#'
#' @param ... For additional parameters to be passed on
#'
#' @name fetch
NULL

#' @rdname fetch
#' @family fetchers
#' @export
fetch_data <- function(x, name) {

	y <- attributes(x)$data_table
	data_name <- y$data_name[y$name == name]
	z <- attributes(x)$data_list
	index <- match(data_name, z$data_name)
	data <- z$data[[index]]
	data

}

#' @rdname fetch
#' @export
fetch_data_name <- function(x, name) {

	y <- attributes(x)$data_table
	data_name <- y$data_name[y$name == name]
	data_name

}

#' @rdname fetch
#' @export
fetch_strata <- function(x, name) {

	y <- attributes(x)$data_table
	strata <- y$strata[y$name == name]
	strata

}

#' @rdname fetch
#' @export
fetch_test <- function(x, name) {

	y <- attributes(x)$test_table
	test <- unique(y$test[y$name == name])[[1]]
	test

}

#' @rdname fetch
#' @export
fetch_test_opts <- function(x, name) {

	y <- attributes(x)$test_table
	test_opts <- unique(y$test_opts[y$name == name])[[1]]
	test_opts

}

#' @rdname fetch
#' @export
fetch_combination <- function(x, name) {

	y <- attributes(x)$test_table
	combination <- y$combination[y$name == name]
	combination

}

#' @rdname fetch
#' @export
fetch_formulae <- function(x, name) {

	formulae <- x$formulae[x$name == name]
	formulae

}

#' @rdname fetch
#' @export
fetch_call <- function(x, name) {

	y <- attributes(x)$test_table
	cl <- y$call[y$name == name][[1]]
	cl

}

#' @rdname fetch
#' @export
fetch_tidy <- function(x, name) {

	res <- x$tidy[x$name == name]
	res

}

#' @rdname fetch
#' @export
fetch_raw <- function(x, name) {

	res <- x$fit[x$name == name]
	res

}

#' @rdname fetch
#' @export
fetch_names <- function(x, ...) {

	opts <- list(...)
	named_args <- names(opts)
	name_list <- list()

	for (i in 1:length(opts)) {
		for (j in named_args) {
			y <- attr(x, "test_table")
			name_list[[i]] <- y$name[y$combination == opts[[j]]]
		}
	}

	# Return vector of names
	unlist(name_list)

}

