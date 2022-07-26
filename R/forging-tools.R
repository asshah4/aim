# Parameter Level Estimates ----------------------------------------------------

#' Holding a list of parameter_estimates
#' @keywords internal
#' @noRd
new_parameter_estimates <- function(x = list()) {
	new_list_of(x, ptype = tibble(), class = "parameter_estimates")
}

#' @export
parameter_estimates <- function(x = unspecified(), ...) {

	if (length(x) == 0) {
		return(new_parameter_estimates())
	}

	if (class(x)[1] == "model_archetype") {
		pe <- possible_tidy(x)
		nms <-
			vec_data(x)[-1] |>
			{
				\(.x) {
					.y <- field(.x$fmls, "formula")
					.z <- paste0()
				}
			}()
	}

	# Return list_of
	new_parameter_estimates(pe)
}

#' @export
vec_ptype_full.parameter_estimates <- function(x, ...) "parameter_estimates"

#' @export
vec_ptype_abbr.parameter_estimates <- function(x, ...) "par_est"

# Model Level Statistics -------------------------------------------------------

#' Holding a list of model level statistics
#' @keywords internal
#' @noRd
new_model_info <- function(x = list()) {
	new_list_of(x, ptype = tibble(), class = "model_info")
}

#' @export
model_info <- function(x = unspecified(), ...) {

	if (length(x) == 0) {
		return(new_model_info())
	}

	if (class(x)[1] == "model_archetype") {
		me <- possible_glance(x)
	}

	# Return list_of
	new_model_info(me)
}

#' @export
vec_ptype_full.model_info <- function(x, ...) "model_info"

#' @export
vec_ptype_abbr.model_info <- function(x, ...) "md_info"

# Data Lists -------------------------------------------------------------------

#' Data frame holding list
#' @keywords internal
#' @noRd
new_data_list <- function(x = list()) {
	new_list_of(x, ptype = tibble(), class = "data_list")
}

#' @export
data_list <- function(x = unspecified(), ...) {

	if (length(x) == 0) {
		return(new_data_list())
	}

	vec_assert(x, ptype = list())
	new_data_list(x)
}

#' @export
vec_ptype_full.data_list <- function(x, ...) "data_list"

#' @export
vec_ptype_abbr.data_list <- function(x, ...) "dl"


# Modify Forge Inputs and Outputs ----------------------------------------------

#' Hammers the ellipsis arguments into a flattened list
#' @keywords internal
#' @noRd
hammer <- function(object, name) {

	stopifnot(inherits(object, "list"))
	contents <- sapply(object, function(.x) { class(.x)[1] })
	if (!any(
		contents %in% c(
			"formula",
			"formula_archetype",
			"model_archetype",
			arcane:::supported_models
		)
	)) {
		message("Every object entered is not appropriate for the `forge`.")
	}

	mtl <- list() # Empty list to append to

	for (i in seq_along(object)) {
		x <- object[[i]]
		m <- list() # Temporary list to append to the master list above
		nm <- character() # Temporary character name holder

    if (class(x)[1] == "formula") {
	    # Formulas
  		z <- fmls(x, order = 2:3)
  		m <- append(m, as.list(z))
    } else if (class(x)[1] == "formula_archetype") {
    	# Fmls archetypes
    	m <- append(m, as.list(x))
    } else if (class(x)[1] %in% arcane:::supported_models) {
    	# Standard modeling objects
    	z <- md(x)
    	m <- append(m, as.list(z))
    } else if (class(x)[1] == "model_archetype") {
    	# Model archetypes
    	m <- append(m, as.list(x))
    	nm <- vec_data(x)$name
    } else {
    	# For unknown objects (would include a warning)
    	m <- append(m, as.list(x))
    }

    # Creating new names for everything but model archetypes
		n <- length(m)
		if (length(nm) == 0) {
			nm <- paste0(name[i], "_", 1:n)
		}

		if (length(m) > 1) {
			names(m) <- nm
		} else {
			names(m) <- name[i]
		}

		mtl <- append(mtl, m)
	}

	# Return completed/reshaped list
	mtl

}

#' Temper objects from the forge
#'
#' @param object A model `forge` object
#' @param f A formula or formula_archetype object
#' @export
temper <- function(object, ...) {

	# Validate
	stopifnot(inherits(object, "forge"))

	# Trim away unnecessary variables
	# Unnest the parameter estimates and model information
	object |>
		subset(run == TRUE) |>
		subset(select = c(type, subtype, name, number, formula, outcome, exposure, mediator, strata, level, parameter_estimates, model_info, terms)) |>
		dplyr::mutate(model_info = purrr::map(model_info, tibble::as_tibble)) |>
		tidyr::unnest(model_info) |>
		dplyr::rename(
			global.statistic = statistic,
			global.p.value = p.value
		) |>
		dplyr::mutate(parameter_estimates = purrr::map(parameter_estimates, tibble::as_tibble)) |>
		tidyr::unnest(parameter_estimates)

}
