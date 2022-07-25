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


# Flatten arguments down to a list ---------------------------------------------

#' Hammers the ellipsis arguments into a flattened list
#' @keywords internal
#' @noRd
hammer <- function(object, name) {

	stopifnot(inherits(object, "list"))
	contents <- sapply(object, function(.x) { class(.x)[1] })
	if (!any(contents %in% c("formula",
													 "formula_archetype",
													 "model_archetype",
													 "lm",
													 "glm",
													 "model_fit"))) {
		stop("Every object entered is not appropriate for the `forge`.")
	}

	mtl <- list() # Empty list to append to

	for (i in seq_along(object)) {
		x <- object[[i]]
		m <- list() # Temporary list to append to the master list above

    # Formulas
    if (class(x)[1] == "formula") {
  		z <- fmls(x, order = 2:3)
  		m <- append(m, as.list(z))
    }

    # This may be fmls, fmls "to be", or model archetype lists
    if (class(x)[1] == "formula_archetype") {
    	m <- append(m, as.list(x))
    }

    if (class(x)[1] %in% c("lm", "glm", "model_fit", "coxph")) {
    	z <- md(x)
    	m <- append(m, as.list(z))
    }

    if (class(x)[1] == "model_archetype") {
    	m <- append(m, as.list(x))
    }

    # Naming
		n <- length(m)
		nm <- paste0(name[i], "_", 1:n)

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


# Draw out components from a forge ---------------------------------------------

#' Draw out components from the forge
#'
#' @param x A model `forge` object
#' @param f A formula or formula_archetype object
#' @export
draw <- function(x, f) {

}
