# Marksman's Aim ====

#' @title Aiming For Target Hypotheses
#'
#' @description This creates the hypothesis that are being prepared for
#'   analysis. These contain model specifications and the pipeline, along with
#'   model identifiers.
#'
#' @param bullet Bullet object that is a hypothesis with permutations of how a
#'   model is built, based on the statistical plan.
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
#' a <- aim(b)
#'
#' @importFrom magrittr %>%
#'
#' @export
aim <- function(bullet, ...) {

	# Check to see if this is a single list object or multiple
	if (!methods::is(bullet, "bullet")) {
		stop("The input is not of type `bullet`.",
				 call. = FALSE)
	}

	# Bullet: outcomes, exposures, covariates, approach, and models
	# ballistics() will organize the data and include new formulas and model spec

	# Using a map function to create new aim
	aim <-
		bullet %>%
		ballistics() %>%
		new_aim() %>%
		ballistics()

	# Return
	return(aim)

}

#' @description Construct a new aim, with validation built in
#' @noRd
new_aim <- function(ammo) {

	# Confirm that the "ammo" is a table in an aims format
	stopifnot(is.data.frame(ammo))

	# Final structure defined
	structure(ammo, class = c("aim", class(ammo)))

}
