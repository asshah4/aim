# Marksman's Aim ====

#' @title Aiming For Targeted Hypotheses
#'
#' @description The `aim` function arranges the characteristics returned from
#'   `bullet` objects into a table of hypothesis that can be tested in the model
#'   specifications. It creates model identifiers, and collects both the
#'   pre-analysis inputs and the post-analysis attributes. It allows for
#'   organizing several related hypotheses together.
#'
#' @param bullets A `list` of `bullet` objects (preferably named) that will be
#'   used to generate a specific aim. Each bullet should be from the same
#'   dataset to be able to run the analysis in the pre-specified models.
#'
#' @param ... For extensibility
#'
#' @return An `aim` object that contain all the relevant hypothesis that were
#'   pre-specified, including model specifications from `tidymodels`, in a table
#'   format.
#'
#' @examples
#' library(parsnip)
#' f <- mpg + cyl ~ wt + hp + gear
#' lm_mod <- linear_reg() %>% set_engine("lm")
#' b <- bullet(f, exposure = "wt", approach = "sequential", model = lm_mod)
#' a <- aim(list(lin = b))
#'
#' @importFrom magrittr %>%
#'
#' @export
aim <- function(bullets, ...) {

	# Check to see class/type of the list
	if (!(methods::is(bullets, "list") & methods::is(bullets[[1]], "bullet"))) {
		stop("The input is not of a list of `bullet` objects.",
				 call. = FALSE)
	}

	# Bullet: outcomes, exposures, covariates, approach, and models
	# ballistics() will organize the data and include new formulas and model spec
	# Using a map function to create new aim
	aim <-
		bullets %>%
		purrr::map(function(x) {
			ballistics(x)
		}) %>%
		dplyr::bind_rows(., .id = "ammo") %>%
		new_aim() %>%
		ballistics() %>%
		suppressMessages()

	# Return
	return(aim)

}

#' @description Construct a new aim, with validation built in
#' @noRd
new_aim <- function(ammo) {

	# Confirm that the "ammo" is a table in an aims format
	stopifnot(is.data.frame(ammo))

	# Check to see if already has aim class attached
	if ("aim" %in% class(ammo)) {
		structure(ammo, class = class(ammo))
	} else {
		structure(ammo, class = c("aim", class(ammo)))
	}

}
