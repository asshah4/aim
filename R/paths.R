#' Expand Paths of a Formula
#'
#' This presumes that all covariates are confounders are potential confounders.
#'
#' @param formula an object of class `formula`
#' @param ... For extensibility
#'
#' @export
expand_paths <- function(formula, ...) {

	# Basic terms
	out <- as.character(formula[[2]])
	exp <- labels(stats::terms(formula))[[1]]

	# Each covariate is presumed a confounder
	# Each confounder is the parent of the exposure and the parent of the outcome
	# Thus, each non-exposure term has two relationships
	con <-
		labels(stats::terms(formula)) %>%
		setdiff(., exp)

	# Formula list
	paths <- list()

	# Basic exposure-outcome pattern
	paths[[1]] <- stats::formula(paste(out, exp, sep = " ~ "))

	# Confounders
	if (length(con) > 0) {for (i in 1:length(con)) {
		paths <-
			paths %>%
			append(., stats::formula(paste(out, con[i], sep = " ~ "))) %>%
			append(., stats::formula(paste(exp, con[i], sep = " ~ ")))
	}}

	# Return list of paths
	unique(paths)

}

#' Map Out A Directed Acyclic Graph
#'
#' This function converts a hypothesis into a `dagitty` object (or
#' `tidy_dagitty` if requested). This can subsequently be passed onto the
#' [ggdag::ggdag()] function for additional plotting.
#'
#' @return `dagitty` or `tidy_dagitty` object
#'
#' @param study A `study` object
#' @param name The name of a hypothesis added to the study
#' @param tidy Defaults to FALSE, thus returning a `dagitty` object. If TRUE, then will return a `tidy_dagitty` object.
#' @importFrom rlang !!!
#' @export
map_dag <- function(study, name, tidy = FALSE) {

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
