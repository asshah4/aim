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
#' @examples
#' library(magrittr)
#' octobeast <-
#'   octomod() %>%
#'   core(iris) %>%
#'   arm(
#'     title = "flowers_test",
#'     f = Sepal.Length ~ Sepal.Width,
#'     exposure = NULL,
#'     pattern = "direct",
#'     approach = "t.test"
#'   ) %>%
#'   equip()
#'
#' @export
octomod <- function() {

	# The octomod should have a basic structure that is shared.
	# Common data, families of hypotheses, and results of these tests.
	octomod <- list(
		core = list(),   # The core data to be used throughout
		arms = list(),   # The hypothesis families that are supplied
		equipment = list()  # The tested hypothesis and outcomes, tidied
	)

	# Return
	octomod <- new_octomod(octomod)
}

#' @description Create an object of class `octomod`
#' @noRd
new_octomod <- function(octomod) {

	# Confirm that the object is a list structure
	stopifnot(is.list(octomod))

	# Check to see if core exists

	# Final structure defined
	structure(octomod, class = "octomod")

}


#' @description Generic print method
#' @param x Object of class `octomod`
#' @param ... arguments to pass on
#' @noRd
#' @export
print.octomod <- function(x, ...) {

	# Intro
	cat(
		"\nThis `octomod` has the following components.",
		"\n"
	)

	# Core
	cat(
		"\nCore Variables",
		"\n--------------",
		"\n"
	)
	print(names(x$core))

	# Arms
	cat(
		"\nArms",
		"\n----",
		"\n"
	)
	print(names(x$arms))

	# Equipment
	cat(
		"\nEquipment",
		"\n----",
		"\n"
	)
	print(names(x$equipment))

	# If its been appropriately equipmentted
	if (length(x$core) > 0 &&
			length(x$arms) == length(x$equipment) &&
			length(x$arms) > 0) {
		cat(
			"\n------------------------------------------------",
			"\nThis `octomod` is an OCTOBEAST, armed and ready!",
			"\n------------------------------------------------"
		)
	}

}
