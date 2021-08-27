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
#' @name retrieval
NULL

#' @rdname retrieval
#' @export
fetch_data <- function(x, name) {

	y <- attributes(x)$data_table
	data_name <- y$data_name[y$name == name]
	z <- attributes(x)$data_list
	index <- match(data_name, z$data_name)
	data <- z$data[[index]]
	data

}

#' @rdname retrieval
#' @export
fetch_data_name <- function(x, name) {

	y <- attributes(x)$data_table
	data_name <- y$data_name[y$name == name]
	data_name

}

#' @rdname retrieval
#' @export
fetch_strata <- function(x, name) {

	y <- attributes(x)$data_table
	strata <- y$strata[y$name == name]
	strata

}

#' @rdname retrieval
#' @export
fetch_test <- function(x, name) {

	y <- attributes(x)$test_table
	test <- unique(y$test[y$name == name])[[1]]
	test

}

#' @rdname retrieval
#' @export
fetch_test_opts <- function(x, name) {

	y <- attributes(x)$test_table
	test_opts <- unique(y$test_opts[y$name == name])[[1]]
	test_opts

}

#' @rdname retrieval
#' @export
fetch_combination <- function(x, name) {

	y <- attributes(x)$test_table
	combination <- y$combination[y$name == name]
	combination

}

#' @rdname retrieval
#' @export
fetch_formulae <- function(x, name) {

	y <- x$model_map
	formulae <- y$formulae[y$name == name]
	formulae

}

#' @rdname retrieval
#' @export
fetch_call <- function(x, name) {

	y <- attr(x, "test_table")
	cl <- y$call[y$name == name][[1]]
	cl

}

#' @rdname retrieval
#' @export
fetch_tidy <- function(x, name) {

	y <- x$model_map
	res <- y$tidy[y$name == name]
	res

}

#' @rdname retrieval
#' @export
fetch_raw <- function(x, name) {

	y <- x$model_map
	res <- y$fit[y$name == name]
	res

}

#' @rdname retrieval
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

#' Study Modifications
#'
#' Either add, update, or remove components or attributes of a `study`
#'
#' @return A `study` object
#'
#' @param study Object of `study` class
#' @param hypothesis Object of `hypothesis` class
#' @param name Name of `hypothesis`, given from parent function
#' @param ... additional arguments to pass along
#'
#' @name modify_study
#' @keywords internal
NULL

#' @rdname modify_study
#' @keywords internal
#' @export
add_study_formula <- function(study, hypothesis, name) {

	formula <- stats::formula(stats::terms(hypothesis))
	combination <- attr(hypothesis, "combination")
	parameters <- expand_formula(formula, combination, table = TRUE)
	n <- nrow(parameters)

	# Major parameters should be passed along
	for (i in 1:n) {
		study$model_map <-
			study$model_map %>%
			tibble::add_row(
				name = name,
				number = parameters$number[i],
				outcomes = parameters$outcomes[i],
				exposures = parameters$exposures[i],
				formulae = parameters$formulae[i]
			) %>%
			unique()

		# Store the key variables
		attributes(study)$var_table <-
			attributes(study)$var_table %>%
			tibble::add_row(
				name = name,
				outcomes = parameters$outcomes[i],
				exposures = parameters$exposures[i],
				confounders = list(NA)
			) %>%
			unique()
	}

	# Return
	invisible(study)

}

#' @rdname modify_study
#' @keywords internal
#' @export
add_study_path <- function(study, name) {

	# Must be called AFTER the formulas have been decomposed
	# List of appropriate models
	models <-
		study$model_map %>%
		.[.$name == name, ] %>%
		.[.$number == max(.$number), ] %>%
		unique()

	for (i in 1:nrow(models)) {

		# Terms
		f <- models[i, ]$formulae[[1]]
		exp <- models[i, ]$exposures[[1]]
		out <- as.character(f[[2]])

		# Path formulas
		paths <- expand_paths(f)

		# Create paths and update original data
		for (j in 1:length(paths)) {
			study$path_map <-
				study$path_map %>%
				tibble::add_row(
					name = name,
					outcomes = out,
					exposures = exp,
					formulae = paths[j],
					from = as.character(paths[[j]][[3]]),
					direction = "->",
					to = as.character(paths[[j]][[2]]),
					type = NA,
					related = NA
				) %>%
				.[!duplicated(.[1:6]), ]
		}
	}

	# Return
	invisible(study)
}

#' @rdname modify_study
#' @keywords internal
#' @export
add_study_test <- function(study, hypothesis, name) {

	# Update test information
	attr(study, "test_table") <-
		attr(study, "test_table") %>%
		tibble::add_row(
			name = name,
			call = deparse(stats::formula(stats::terms(hypothesis))),
			test = list(attributes(hypothesis)$test),
			test_opts = attributes(hypothesis)$test_opts,
			combination = attributes(hypothesis)$combination,
			type = utils::tail(class(attributes(hypothesis)$test), 1)
		) %>%
		unique()

	# Return
	invisible(study)

}


#' @rdname modify_study
#' @keywords internal
#' @export
add_study_data <- function(study, hypothesis, name) {

	# Data table for linking hypothesis to data
	attributes(study)$data_table <-
		attributes(study)$data_table %>%
		tibble::add_row(
			name = name,
			data_name = attributes(hypothesis)$data_name,
			strata = attributes(hypothesis)$strata
		) %>%
		unique()

	# Data list (to minimize too many data sets)
	attributes(study)$data_list <-
		attributes(study)$data_list %>%
		tibble::add_row(
			data_name = attributes(hypothesis)$data_name,
			data = list(attributes(hypothesis)$data)
		) %>%
		unique()

	# Return study
	invisible(study)

}

#' @rdname modify_study
#' @keywords internal
#' @export
add_study_status <- function(study,
														 hypothesis,
														 name,
														 run = FALSE,
														 error = FALSE,
														 path = FALSE,
														 origin = NA) {

	# Modify the Status Table
	attributes(study)$status_table <-
		attributes(study)$status_table %>%
		tibble::add_row(
			name = name,
			run = run,
			path = path,
			error = error,
			origin = origin
		) %>%
		unique()

	# Return study
	invisible(study)

}

#' @rdname modify_study
#' @keywords internal
#' @export
update_study_status <- function(study, name, ...) {

	# Match call
	mc <- match.call(expand.dots = TRUE)
	changes <- list(...)

	# Get named study status
	x <- attr(study, "status_table")
	y <- x[x$name == name, ]

	# Names
	new_names <- names(changes)
	old_names <- names(y)

	# Update status
	for (i in new_names) {
		y[[i]] <- changes[[i]]
	}

	# Replace in study
	x[x$name == name, ] <- y
	attr(study, "status_table") <- x

	# Return invisibly
	invisible(study)

}

