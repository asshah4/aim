# Fire ====

#' @title Firing using the Aims at Target Hypotheses
#'
#' @description As the analysis time can be quite long, this function separates
#'   out model runtime until after the workflows for analysis are completed.
#'   This function works on a single `aim` object at a time, allowing for list
#'   methods such as [purrr::map] to apply.
#'
#' @param aim A single `aim` object
#'
#' @param data Data frame that contains the variables that were planned for in
#'   the specific aims generated prior.
#'
#' @param ... For extensibility
#'
#' @return Returns the fitted models in an expanded table, still of the `aim` class
#'
#' @importFrom magrittr %>%
#'
#' @export
fire <- function(aim, data, ...) {

	# Check correct aims
	if (!methods::is(aim, "aim")) {
		stop("Please use an single `aim` object.", call. = FALSE)
	}

	# Make sure its passing the checkpoint
	if (FALSE %in% aim$checkpoint) {
		stop("This `aim` has not passed all the checkpoints.", call. = FALSE)
	}

	### Fire!

	# Splits the data based on if it passed the checkpoint
	target <-
		aim %>%
		dplyr::mutate(fit = purrr::map2(
			model_spec, formulas,
			~ fit(.x, .y, data = data))
		) %>%
		new_aim()

	# Return
	return(target)

}

