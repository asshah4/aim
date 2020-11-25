# Marksman's Aim ====

#' @title Aiming For Target Hypothesis
#' @description This creates the hypothesis that are being prepared for
#'   analysis. These contain model specifications and the workflows, along with
#'   model identifiers.
#' @param bullets List object of bullets that are to be thought of as the
#'   statistical plan, as defined by each individual bullet. The list can be
#'   named to make it simpler.
#' @param ... For extensibility
#' @return List of `aims` that contain all the relevant hypothesis that were
#'   pre-specified, including workflows and model specifications from
#'   `tidymodels`
#' @examples
#' library(card)
#' library(parsnip)
#' data(geh)
#' f <- svg_mag + qrs_tang ~ lab_hba1c + age + sex + bmi + cad + htn
#' lm_mod <- linear_reg() %>% set_engine("lm")
#' b1 <- bullet(f, exposure = "lab_hba1c", approach = "sequential", model = lm_mod)
#' b2 <- bullet(f, exposure = "lab_hba1c", approach = "parallel", model = lm_mod)
#' bullets <- list(seq = b1, par = b2)
#' @importFrom magrittr %>%
#' @export
aim <- function(bullets, ...) {

	# Check to see if this is a single list object or multiple
	if (!methods::is(bullets, "list")) {
		stop("Please wrap the `bullets` argument as a vector of `bullet` objects).",
				 call. = FALSE)
	}

	# Bullets: outcomes, exposures, covariates, approach, and models
	# ballistics() will organize the data and include new formulas and model spec

	# Using a map function to create new aim
	# Each object will be of the aim class, collated into a list
	aims <-
		purrr::map(bullets, function(x) { ballistics(x) }) %>%
		purrr::map(., new_aim)

	# Return
	return(aims)

}

#' @description Construct a new aim, with validation built in
#' @noRd
new_aim <- function(ammo) {

	# Confirm that the "ammo" is a table in an aims format
	stopifnot(is.data.frame(ammo))

	# Final structure defined
	structure(ammo, class = c("aim", class(ammo)))

}
