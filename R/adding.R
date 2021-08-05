#' Adding Data
#'
#' If a hypothesis does not have a data, it can be added subsequently. This can be done in a pipe, either to the hypothesis itself, or to a framework, to allow for flexibility.
#'
#' @return Returns a `hypothesis` or `framework` object with the prescribed data added back on, which can be seen by calling `print(framework)` or `attributes(hypothesis)$data`.
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

	# Validate
	validate_hypothesis_exists(x, name)

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

	# Rows should be added, and then subsequently data should be added
	.dt <- attributes(x)$data_table

	if (nrow(.dt) == 0) {
		.dt <-
			.dt %>%
			tibble::add_row(
				name = name,
				data_name = .data_name,
				strata = .strata
			)
		.dt$data_list <- list(.data)
	} else if (nrow(.dt) > 0) {
		.dt <-
			.dt %>%
			tibble::add_row(
				name = name,
				data_name = .data_name,
				strata = .strata
			)
		.dt$data_list[[nrow(.dt)]] <- .data
	}

	# Return the attributes back to the framework
	attributes(x)$data_table <- .dt

	# Make sure data names are characters for merging (versus NA as logical)
	x <- within(x, {
		data_name =
			ifelse(is.character(data_name), data_name, as.character(data_name))
	})

	# If adding data to new data set
	if (nrow(.dt) == 1) {
		x$data_name[x$name == name] <- .dt$data_name
	}
	# If data already present, then must be more selective
	else if (nrow(.dt) > 1) {
		# Duplicate original data frame
		y <- x[x$name == name, ]

		y$data_name <-
			.dt$data_name[.dt$name == name & .dt$data_name != unique(x$data_name)]

		# Bind together new rows for all hypothesis/data combinations
		x <- do.call("rbind", list(x, y))
	}

	# Return framework
	x
}

