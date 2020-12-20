#' Create a Many-Armed Structure to Hold Hypotheses
#'
#' This creates a central structure that will have many "arms" based on the
#' number of hypotheses that exist for a certain research question. The
#' structure manifests the promise that a single data set can be used to pose
#' multiple hypotheses. These hypothesis should be pre-specified to help
#' structure a research project. At any point, the structure of hypotheses
#' should be agile and malleable.
#'
#' The term `octomod` was used as a play on the concept of how many arms an
#' octopus has, and in this case, it can be used to hold specific models or
#' ideas. This *modeling beast* can help us tackle complex research projects
#' sensibly and flexibly.
#'
#' @return `octomod` object
#'
#' @export
octomod <- function() {

	# The octomod should have a basic structure that is shared.
	# Common data, families of hypotheses, and results of these tests.
	octomod <- list(
		core = list(),   # The core data to be used throughout
		arms = list(),   # The hypothesis families that are supplied
		outfit = list()  # The tested hypothesis and outcomes, tidied
	)

	# Return
	octomod <- new_octomod(octomod)
}

#' @description Create an object of class `octomod`
#' @noRd
new_octomod <- function(octomod) {

	# Confirm that the object is a list structure
	stopifnot(is.list(octomod))

	# Final structure defined
	structure(octomod, class = "octomod")

}
