#' Adding Data
#'
#' If a hypothesis does not have a data, it can be added subsequently. This can be done in a pipe, either to the hypothesis itself, or to a framework, to allow for flexibility.
#'
#' @param x Either a `hypothesis` or `framework` object
#'
#' @family helpers
#' @export
add_data <- function(x, ...) {
	UseMethod("add_data")
}

#' Add data to the hypothesis
#'
#' @param .data Data set to be linked to this hypothesis. The argument can
#'   either be a character string or a data frame, and defaults to NULL. If
#'   `.data` is a character string, it assumes that a data frame will be added
#'   later (or is already available in the `framework` object). If the `.data`
#'   is a data frame, it takes the name of the given data. It is not necessary
#'   to include a data argument, but the hypothesis cannot be tested until a
#'   data set is provided. This allows for flexibility and lightweight
#'   manipulation of the `hypothesis` object.
#'
#' @param .strata How the data should be split or stratified. References the
#'   name of the data given in that the models will be with fit against,
#'   splitting the data into subsets. This helps to perform hypothesis testing
#'   on subsets or strata of the data. It defaults to NULL (which means the full
#'   data will be used). This argument will only be incorporated if `.data` is
#'   present.
#'
#' @rdname add_data
#' @export
add_data.hypothesis <- function(x, .data, .strata = NULL) {
	# Warn if data is already present

	# The data name helps to link the datasets to the tests and hypothesis
	if (!is.null(.data)) {
		.data_name <- deparse(substitute(.data))
	} else {
		.data_name <- NULL
	}

	# Update attributes
	x <-
		x %>%
		.set_hypothesis_data(.data, .data_name, .strata)

	# Return hypothesis
	x
}

#' Add data to the hypothesis that is a part of a framework
#'
#' @param name Character vector of which hypothesis the data should be added to.
#'
#' @rdname add_data
#' @export
add_data.framework <- function(x, name, .data, .strata = NULL) {
	# Warn if data is already present

	# The data name helps to link the datasets to the tests and hypothesis
	if (!is.null(.data)) {
		.data_name <- deparse(substitute(.data))
	} else {
		.data_name <- NULL
	}

	# Strata cannot be NULL when adding
	if (is.null(.strata)) {
		.strata <- NA
	}

	# Adds a row
	#attributes(x)$data_table <- attributes(x)$data_table %>%
	attributes(x)$data_table %>%
		tibble::add_row(
			hypothesis_name = name,
			data = list(.data),
		)
			data_name = .data_name,
			strata = .strata
		)

	# Return framework
	x
}

