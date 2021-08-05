# Retrieve Functions ----

#' Retrieve, Modify, and Destroy Components From `framework` Object
#'
#' @description
#'
#' These functions are essentially helper functions to simplify the manipulation
#' of `framework` objects. They may retrieve or modify or destroy components of
#' the `framework` object as described.
#'
#' __Retrieval functions__:
#'
#' * `get_data()` retrieves the data set associated with a specified
#' hypothesis and data name
#'
#' * `get_test()` retrieves the test associated with a hypothesis
#'
#' * `get_formulae()` retrieves the list of formulaes associated with
#' a hypothesis and data set.
#'
#' __Reporting functions__
#'
#' * `report_stage()` creates a message to identify at what stage a
#' hypothesis has been developed to.
#'
#' __Update functions__:
#'
#' * `update_status()` modifies the components of a framework, usually
#' used internally to help update the stage/status of the specified object
#'
#' @return The `get_*()` functions return the named object from the `framework`.
#'   The `update_*()` functions return the entire framework after modification.
#'
#' @param x `framework` object
#'
#' @param name Name of `hypothesis` object to pull components from
#'
#' @param ... For optional parameters
#'
#' @name retrieval
NULL

#' @rdname retrieval
#' @export
get_data <- function(x, name) {

	y <- attributes(x)$data_table
	data <- y$data_list[[y$name == name]]

	# Returns data
	data

}

#' @rdname retrieval
#' @export
get_test <- function(x, name) {

	y <- attributes(x)$test_table
	if (y$type[y$name == name] == "model_spec") {
		test <- y$test[[y$name == name]]
	}

	# Return
	test

}

#' @rdname retrieval
#' @export
get_combination <- function(x, name) {

	y <- attributes(x)$test_table
	combination <- y$combination[y$name == name]

	# Return
	combination

}

#' @rdname retrieval
#' @export
get_formulae <- function(x, name) {

	formulae <- x$formulae[x$name == name]

	# Return
	formulae

}

#' @rdname retrieval
#' @export
get_parameters <- function(x, name) {

	pars <- x$tidy[x$name == name]

	# Return
	pars
}

#' @rdname retrieval
#' @export
update_status <- function(x, name, stage = NA, run = FALSE) {

	status <- attributes(x)$status_table
	status$run <- run
	status$stage <- stage
	attributes(x)$status_table <- status

	# Return
	invisible(x)
}

# Linking Functions ----

#' Linking Functions Between Hypothesis and Framework
#'
#' These are all internal functions to help attach components to the `framework` object, and are generally not exposed to the user.
#'
#' @param framework Object of `framework` class
#' @param hypothesis Object of `hypothesis` class
#' @param name Name of `hypothesis`, given from parent function
#' @name link_framework
#' @keywords internal
NULL

#' @rdname link_framework
#' @keywords internal
.link_hypothesis <- function(framework, hypothesis, name) {

	# Identify number of sub-hypotheses
	parameters <- attributes(hypothesis)$parameters
	formulae <- attributes(hypothesis)$formulae
	n <- nrow(parameters)

	# Major parameters should be passed along
	for (i in 1:n) {
		framework <- framework %>%
			tibble::add_row(
				name = name,
				number = parameters$number[i],
				outcome = parameters$outcomes[i],
				exposure = parameters$exposures[i],
				formulae = list(formulae[[i]])
			)
	}

	# Return
	invisible(framework)
}

#' @rdname link_framework
#' @keywords internal
.link_test <- function(framework, hypothesis, name) {
	# Test, test type, and test options should be passed along
	attributes(framework)$test_table <-
		attributes(framework)$test_table %>%
		tibble::add_row(
			name = name,
			call = list(stats::formula(stats::terms(hypothesis))),
			test = list(attributes(hypothesis)$test),
			test_opts = ifelse(is.null(attributes(hypothesis)$test_opts), NA, test_opts),
			combination = attributes(hypothesis)$combination,
			type = tail(class(attributes(hypothesis)$test), 1)
		)

	# Return
	invisible(framework)
}

#' @rdname link_framework
#' @keywords internal
.link_data <- function(framework, hypothesis, name) {

	# Get data name
	data_name <- names(attributes(hypothesis)$data)

	attributes(framework)$data_table <-
		attributes(framework)$data_table %>%
		tibble::add_row(
			name = name,
			data_name = data_name,
			data_list = list(attributes(hypothesis)$data[[data_name]]),
			strata = attributes(hypothesis)$strata[[data_name]]
		)

	# Return framework
	invisible(framework)
}

#' @rdname link_framework
#' @keywords internal
.link_status <- function(framework, hypothesis, name) {

	# Add hypothesis and data_name to status table
	attributes(framework)$status_table <-
		attributes(framework)$status_table %>%
		tibble::add_row(
			name = name,
			run = FALSE,
			error = NA,
			stage = NA
		)


	# Return
	invisible(framework)
}

