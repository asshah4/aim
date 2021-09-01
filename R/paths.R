#' Expand paths of a formula
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

