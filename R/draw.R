#' Convert paths to a `tidygraph`
#'
#' This is a wrapper function to convert a `study` to a `tbl_graph` object for
#' use with [{ggraph}](https://ggraph.data-imaginist.com/) and
#' [{tidygraph}](https://tidygraph.data-imaginist.com/reference/index.html).
#'
#' @return a `tbl_graph` object
#'
#' @param study A `study` object
#'
#' @inheritParams tidygraph::as_tbl_graph
#'
#' @param ... For extensibility
#'
#' @rdname as_tbl_graph
#' @importFrom tidygraph as_tbl_graph
#' @export
as_tbl_graph.study <- function(study, directed = TRUE, ...) {

	p <- study$path_map
	tidygraph::as_tbl_graph(p, directed = directed, ...)

}


#' Extract a `dagitty` object
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' This function converts a hypothesis into a `dagitty` object (or
#' `tidy_dagitty` if requested). This can subsequently be passed onto the
#' [ggdag::ggdag()] function for additional plotting.
#'
#' @return `dagitty` or `tidy_dagitty` object
#'
#' @param study A `study` object
#'
#' @param name The name of a hypothesis added to the study
#'
#' @param tidy Defaults to FALSE, thus returning a `dagitty` object. If TRUE,
#'   then will return a `tidy_dagitty` object.
#'
#' @importFrom rlang !!!
#' @family extractors visualizers
#' @export
extract_dagitty <- function(study, name, tidy = FALSE) {

	p <- study$path_map
	f <- p$formulae

	exp <- unique(p$exposures[p$name == name])
	out <- unique(p$outcomes[p$name == name])

	if (tidy) {
		rlang::exec(ggdag::dagify, !!!f, exposure = exp, outcome = out) %>%
		ggdag::tidy_dagitty(.)
	} else {
		rlang::exec(ggdag::dagify, !!!f, exposure = exp, outcome = out)
	}

}
