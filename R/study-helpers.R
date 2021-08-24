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

	# Returns data
	data

}

#' @rdname retrieval
#' @export
fetch_test <- function(x, name) {

	y <- attributes(x)$test_table
	if (y$type[y$name == name] == "model_spec") {
		index <- match(name, y$name)
		test <- y$test[[index]]
	}

	# Return
	test

}

#' @rdname retrieval
#' @export
fetch_combination <- function(x, name) {

	y <- attributes(x)$test_table
	combination <- y$combination[y$name == name]

	# Return
	combination

}

#' @rdname retrieval
#' @export
fetch_formulae <- function(x, name) {

	y <- x$model_map
	formulae <- y$formulae[y$name == name]

	# Return
	formulae

}

#' @rdname retrieval
#' @export
fetch_parameters <- function(x, name) {

	y <- x$model_map
	pars <- y$tidy[y$name == name]

	# Return
	pars

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

#' @rdname retrieval
#' @param stage Hypothesis stage
#' @param run If model has been built
#' @param path Variable for path status
#' @export
update_study <- function(x, name, stage = NA, run = FALSE, path = FALSE) {

	if (!is.na(stage)) {
		status <- attributes(x)$status_table
		status$run <- run
		status$stage <- stage
		attributes(x)$status_table <- status
	}

	# Return
	invisible(x)
}

#' Study Modifications
#'
#' @param study Object of `study` class
#' @param hypothesis Object of `hypothesis` class
#' @param name Name of `hypothesis`, given from parent function
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
				outcome = parameters$outcomes[i],
				exposure = parameters$exposures[i],
				formulae = parameters$formulae[i]
			)
	}

	# Return
	invisible(study)

}

#' @rdname modify_study
#' @keywords internal
#' @export
add_study_paths <- function(study, hypothesis, name) {

	models <-
		study$model_map %>%
		.[.$name == name & .$number == max(.$number), ] %>%
		.[c("name", "outcome", "exposure", "formulae")] %>%
		unique()

	for (i in 1:nrow(models)) {

		# Terms
		f <- models[i, ]$formulae[[1]]
		exp <- models[i, ]$exposure[[1]]
		out <- as.character(f[[2]])

		# Path formulas
		paths <- expand_paths(f)

		for (j in 1:length(paths)) {
			study$path_map <-
				study$path_map %>%
				tibble::add_row(
					name = name,
					outcome = out,
					exposure = exp,
					relationship = paths[j],
					term = as.character(paths[[j]][[3]]),
					direction = "->",
					to = as.character(paths[[j]][[2]])
				)
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
			call = list(stats::formula(stats::terms(hypothesis))),
			test = list(attributes(hypothesis)$test),
			test_opts = attributes(hypothesis)$test_opts,
			combination = attributes(hypothesis)$combination,
			type = utils::tail(class(attributes(hypothesis)$test), 1)
		)

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
		)

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
																stage = NA,
																path = FALSE) {

	# Modify the Status Table
	attributes(study)$status_table <-
		attributes(study)$status_table %>%
		tibble::add_row(
			name = name,
			run = run,
			error = error,
			stage = stage,
			path = path
		)

	# Return study
	invisible(study)

}
