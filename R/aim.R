#' @title Aiming For Targeted Hypotheses
#'
#' @description The `aim` function does as described, and aims the `bullet` at
#'   the appropriate target. It allows for additional specification of how the
#'   hypothesis should be tested. It arranges these hypothesis into a table that
#'   outlines the trajectory of that bullet. Each part of the hypothesis ends up
#'   being a single row. It creates model identifiers, and collects both the
#'   pre-analysis inputs and the post-analysis attributes. It allows for
#'   organizing several related hypotheses together.
#'
#' @param `bullet` Either a single or several `bullet` object(s) that will be
#'   used to generate a specific aim. The object names should be informative for
#'   the user. Each bullet should be from the same dataset to be able to run the
#'   analysis in the pre-specified models.
#'
#' @param ... To obtain the additional `bullet` objects, as the number may be variable.
#'
#' @return An `aim` object that contain all the relevant hypothesis that were
#'   pre-specified, including model specifications from `tidymodels`, in a table
#'   format.
#'
#' @examples
#' library(parsnip)
#' f <- mpg + cyl ~ wt + hp + gear
#' lm_mod <- linear_reg() %>% set_engine("lm")
#' blm <- bullet(f, exposure = "wt", approach = "sequential", spec = lm_mod)
#' bhtest <- bullet(hp ~ disp, exposure = "disp", approach = "plain", spec = "t.test", paired = TRUE)
#' b1 <- bullet(Sepal.Length + Sepal.Width ~ Petal.Length, exposure = NULL, approach = "plain", spec = "t.test", paired = TRUE)
#' b2 <- bullet(Sepal.Length + Sepal.Width ~ Petal.Length, exposure = NULL, approach = "plain", spec = cs_reg)
#' a <- aim(list(bmod, bhtest))
#'
#' @importFrom magrittr %>%
#'
#' @export
aim <- function(bullets, ...) {

	# Check to see class/type of the list
	if (class(bullets) != "list" & class(bullets[1]) != "bullet") {
		stop("The input is not a list of `bullet` objects.", call. = FALSE)
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

aim2 <- function(...) {

	# Create list of aims
	mc <- match.call()
	mc[[1]] <- quote(list)
	allargs <- eval(mc)
	names(allargs) <- as.character(mc)[-1]

	# Create the aims table
	allargs %>%
		purrr::map(function(x) {
			ballistics2(x)
		}) %>%
		dplyr::bind_rows(.id = "id")
}

ballistics2 <- function(bullet) {

	# Check
	if (class(bullet) != "bullet") {
		stop("The input is not a single `bullet` object.", call. = FALSE)
	}

	# Major variables
	out <- bullet$out
	exp <- bullet$exp
	pred <- bullet$pred
	approach <- bullet$approach
	num <- length(bullet$pred)
	spec <- bullet$spec
	spec_type <- bullet$spec_type
	spec_pars <- bullet$spec_pars

	# Based on approach
	switch(
		approach,
		plain = {
			tbl <-
				tibble::tibble(model_num = 1:num) %>%
				mutate(vars = purrr::map(model_num, ~ pred[1:.])) %>%
				tidyr::expand_grid(outcomes = out, .)
		},
		sequential = {
			tbl <-
				tibble::tibble(model_num = 1:num) %>%
				mutate(vars = purrr::map(model_num, ~ pred[1:.])) %>%
				tidyr::expand_grid(outcomes = out, .)
		},
		parallel = {
			tbl <-
				tibble::tibble(model_num = 1:num) %>%
				mutate(
					vars = purrr::map(model_num, ~ unique(c(exp, pred[.])))
				) %>%
				tidyr::expand_grid(outcomes = out, .)
		}
	)

	# Now can re-create appropriate formulas
	tbl <-
		tbl %>%
		mutate(formulas = purrr::map_chr(pred, ~paste(unlist(.x), collapse = " + "))) %>%
		mutate(formulas = paste(out, formulas, sep = " ~ ")) %>%
		mutate(formulas = purrr::map(formulas, ~formula(.x))) %>%
		mutate(
			spec = list(spec),
			spec_type = spec_type,
			spec_pars = list(spec_pars)
		)

	# Return
	tbl
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
