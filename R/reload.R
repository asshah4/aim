# Reload the Ammo ====

#' @title Reload either the bullets or the aims
#'
#' @description If, after an `aim` or `bullet` object has been made, it can be
#'   revised or reloaded with new characteristics
#'
#' @param mark Represents either a `bullet` or `aim` object to be analyzed for
#'   further information or characteristics
#'
#'   - A single __bullet__ object.
#'
#'   - A single __aim__ object. If it has been processed by `fire()`, then that
#'   component will be removed. Remember, this exists as one of the named lists
#'   created from the `aim()` function.
#'
#' @param ... Any original parameter from `aim` or `bullet` can be
#'   added/revised. Both require calling the name of the element to be revised.
#'   The function will assess the names and update accordingly. Only a single
#'   element can be modified at a time.
#'
#'   -  If an __aim__ object: c("outcomes", "model_num", "predictors", "models",
#'   "raw")
#'
#'   - If a __bullet__ object: c("outcomes", "predictors", "exposures",
#'   "covariates", "approach", "model"), e.g. `reload(bullet, outcomes =
#'   "new_outcome")`
#'
#' @family reload
#' @return Returns the original `aim` or `bullet` with revised components.
#' @export
reload <- function(mark, ...) {
	UseMethod("reload")
}

#' @export
#' @rdname reload
reload.default <- function(mark, ...) {
  stop("`reload()` is not defined for a '", class(mark)[1], "'.", call. = FALSE)
}

#' @export
#' @rdname reload
reload.bullet <- function(mark, ...) {
	# If bullet
	bullet <- mark
	old_terms <- names(bullet)

	# which terms to update
	new_arg <- list(...)
	new_terms <- names(new_arg)

	if(length(bullet[[new_terms]]) != length(new_arg[[new_terms]]) | length(intersect(old_terms, new_terms)) != 1) {
		stop("The new data given to `reload` is either not the same length or does not match with an existing aim term.", call. = FALSE)
	}

	# Return
	bullet[new_terms] <- new_arg
	return(bullet)
}

#' @export
#' @rdname reload
reload.aim <- function(mark, ...) {

	# If an aim
	# Drop the processed column since its being reloaded
	aim <- mark
	aim <- aim[, -which(names(aim) %in% "proc")]
	old_terms <- names(aim)

	# Which terms to update
	new_arg <- list(...)
	new_terms <- names(new_arg)

	# Make sure the term matches and is compatible
	if(length(aim[[new_terms]]) != length(new_arg[[new_terms]]) | length(intersect(old_terms, new_terms)) != 1) {
		stop("The new data given to `reload` is either not the same length or does not match with an existing aim term.", call. = FALSE)
	}

	# Update and return
	aim[new_terms] <- new_arg
	return(bullet)
}

