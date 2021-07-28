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
#' @param data Data set to be attached to this hypothesis. Not necessary to
#'   create a `hypothesis`, and can be specified or added at a later time. The
#'   name of the data set will be used as the reference variable.
#'
#' @param data_name Name of the dataset that this hypothesis should be tested
#'   against. The data itself will be used to generate the name if it is given
#'   from the *data* argument. If only a character vector is given, the function
#'   will match the `hypothesis` to data already available (or warn if not
#'   available). This flexibility allows for lightweight manipulation of the
#'   `hypothesis` object, and the addition of multiple data sets.
#'
#' @param strata How the data should be split or stratified. References the
#'   name of the data given in that the models will be with fit
#'   against, splitting the data into subsets. This helps to perform hypothesis
#'   testing on subsets or strata of the data. It defaults to NULL (which means
#'   the full data will be used) **experimental**
#'
#' @rdname add_data
#' @export
add_data.hypothesis <- function(x,
																data,
																data_name = deparse(substitute(data))) {
	# Warn if data is already present

	# Forces an update of the data if its available
	attributes(x)$data[[data_name]] <- data

	# Return hypothesis
	x
}

#' Add data to the hypothesis that is a part of a framework
#'
#' @param hypothesis_name Character vector of which hypothesis data frame should be added to. This will replace prior data that was set.
#'
#' @rdname add_data
#' @export
add_data.framework <- function(x,
															 hypothesis_name,
															 data,
															 data_name = deparse(substitute(data))) {
	# Warn if data is already present

	# Forces an update if already present
	attributes(x)$data <- attributes(x)$data %>%
		tibble::add_row(
			hypothesis_name = hypothesis_name,
			data_name = data_name,
			data = list(data)
		)

	# Return framework
	x
}
