#' Draw out the map
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' This experimental function is to integrate with [ggraph::ggraph()] to help look at path relationships in the variables that are described. It may start to resemble a _DAG_ as the program develops.
#'
#' @return a `ggraph` object
#'
#' @param ... For extensibility
#'
#' @export
draw <- function(...) {

}


#' Convert paths to a `tidygraph`
#'
#' This is a wrapper function to convert a `study` to a `tbl_graph` object for use with [{ggraph}](https://ggraph.data-imaginist.com/) and [{tidygraph}](https://tidygraph.data-imaginist.com/reference/index.html).
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
as_tbl_graph.study <- function(x, directed = TRUE, ...) {

	tidygraph::as_tbl_graph(x$path_map, directed = directed, ...)

}


