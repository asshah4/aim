#' Add Core Data to the `octomod`
#'
#' @description
#' Add the shared dataset for this research question to the central `octomod`
#' structure. Whenever this is used, it will replace the prior or old data.
#'
#' `core()` adds a new dataset to the `octomod` object. It checks to see if
#' data is already present.
#'
#' `change_core()` changes the old dataset from the `octomod` object to an
#' updated dataset. It does not re-do the hypothesis testing.
#'
#' @return `octomod` object with core data attached
#'
#' @param octomod Object of class `octomod`
#'
#' @param core Data frame or tibble object
#'
#' @param new_core A modified or new data frame to be used
#'
#' @param ... For extensibility
#'
#' @examples
#' library(magrittr)
#'
#' octomod() %>%
#'   core(mtcars) %>%
#'   change_core(iris)
#'
#' @export
#' @rdname core
core <- function(octomod, core) {

	# Check octomod
	if (!inherits(octomod, "octomod")) {
		stop("The `octomod` object must be of `octomod` class.", call. = FALSE)
	}

	# Check core
	validate_core(core)

	# Check to see if core already exists
	if (length(octomod$core) == 1) {
		stop("A `core` already exists. Use `change_core()` instead.", call. = FALSE)
	}

	# Give the core data to the structure
	octomod[["core"]] <- core

	# Return octomod with core attached
	new_octomod(octomod)

}

#' @export
#' @rdname core
change_core <- function(octomod, new_core, ...) {

	# Check octomod
	if (!inherits(octomod, "octomod")) {
		stop("The `octomod` object must inherit from the `octomod` class.")
	}

	# Check core
	validate_core(new_core)

	# See if core already exists
	if (length(octomod$core) == 0) {
		stop("A `core` does not exist. Use `add_core()` instead.", call. = FALSE)
	}

	# Update core
	octomod[["core"]] <- new_core

	# Return
	new_octomod(octomod)

}

#' @export
#' @rdname core
remove_core <- function(octomod, ...) {

	# Check octomod
	if (!inherits(octomod, "octomod")) {
		stop("The `octomod` object must inherit from the `octomod` class.")
	}

	# Check to see if core ecists
	if (length(octomod$core) == 0) {
		stop("A `core` does not exist. Add with `core()` instead.", call. = FALSE)
	}

	# Remove core
	octomod[["core"]] <- list()

	# Return
	new_octomod(octomod)
}

#' @description Validating method for core components
#' @noRd
validate_core <- function(core) {

	# Check core
	if (!inherits(core, "data.frame") &&
			!inherits(core, "tbl_df") &&
			!inherits(core, "tbl") &&
			!inherits(core, "data.table")) {
		stop("The `core` object must be `df`, `tbl`, `dt` object.", call. = FALSE)
	}

}
