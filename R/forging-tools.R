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
