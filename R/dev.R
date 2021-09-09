# nocov start

#' Strategize the Study Approach
#'
#' A hypothetical way to help create a project map.
#' @noRd
strategize <- function(...) {

}


#' Draw out the map
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' This experimental function is to integrate with [ggraph::ggraph()] to help
#' look at path relationships in the variables that are described. It may start
#' to resemble a _DAG_ as the program develops.
#'
#' @return a `ggraph` object
#'
#' @param seed Sets the seed to be used for reproduciblity of the [[ggraph::create_layout()]] options. Defaults to NULL such that no seed is set.
#' @param layout Controls the layout pattern for the plot. Defaults to
#' @param ... For extensibility
#'
#' @import ggraph
#' @export
draw <- function(study,
								 seed = NULL,
								 layout = "graphopt",
								 size = 8,
								 ...) {

	p <- study$path_map
	m <- study$model_map
	x <- as_tbl_graph(study)

	# Optional seed parameter
	if (!is.null(seed)) {
		set.seed(seed)
	}
	layout <- create_layout(x, layout = "stress")

	ggraph(layout) +
		geom_edge_link(
			aes(start_cap = label_rect(node1.name), end_cap = label_rect(node2.name)),
			arrow = arrow(length = unit(size/2, 'mm'))) +
		geom_node_point(size = size, color = "yellow") +
		geom_node_text(aes(label = name), repel = FALSE) +
		theme_graph()

}

#' Derive Formula with Significant Predictors
#' @export
find_significant <- function(study,
														 name,
														 statistic = "p.value",
														 threshold = 0.05,
														 return_study = FALSE,
														 ...) {

}

# nocov end
