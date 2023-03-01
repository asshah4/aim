#' Models with interaction variables
#'
#' A. Figueiras, J. M. Domenech-Massons, and Carmen Cadarso, 'Regression models:
#' calculating the confidence intervals of effects in the presence of
#' interactions', Statistics in Medicine, 17, 2099-2105 (1998)
#'
#' @export
interaction_estimates <- function(object,
																	exposure,
																	binary,
																	conf.level = 0.95,
																	present = TRUE) {

	if (!class(object) %in% arcana:::template_models) {
		stop("Class of `object` is not supported for extracting interaction estimates.")
	}

	# Get matrix and basic summary
	mat <- stats::model.matrix(object)
	dof <- nrow(mat) - ncol(mat)
	coefs_var <- stats::vcov(object)
	coefs <- stats::coefficients(object)

	# Decision tree for if exponentiation will be needed
	tidy_coefs <- possible_tidy(object)
	decision <-
		all.equal(tidy_coefs$estimate[tidy_coefs$term == exposure],
							coefs[exposure],
							tolerance = 1e-2,
							check.names = FALSE)

	# If binary or interaction is not truly dichotomous, should stop
	stopifnot(is_dichotomous(mat[, binary]))

	# Get interaction term
	it <- paste0(exposure, ":", binary)

	if (present) {
		# Confidence interval
		half_ci <-
			stats::qt(conf.level / 2 + 0.5, dof) *
			sqrt(coefs_var[exposure, exposure] +
					 	coefs_var[it, it] +
					 	2 * coefs_var[exposure, it])
		# Estimates
		est <- unname(coefs[exposure] + coefs[it])

		# Number of obs
		val <- levels(factor(mat[, binary]))[2]
		mini_mat <- mat[mat[, binary] == val, ]

		# Parameters
		pars <- list(est, est - half_ci, est + half_ci, nrow(mini_mat), val)

	} else {
		# Confidence interval
		half_ci <-
			stats::qt(conf.level / 2 + 0.5, dof) * sqrt(diag(coefs_var))
		half_ci <- half_ci[exposure]
		# Estimates
		est <- unname(coefs[exposure])

		# Number of obs
		val <- levels(factor(mat[, binary]))[1]
		mini_mat <- mat[mat[, binary] == val, ]

		# Parameters
		pars <- list(est, est - half_ci, est + half_ci, nrow(mini_mat), val)
	}

	# Return vector of length 5
	# Exponentiate if needed
	names(pars) <- c("estimate", "conf.low", "conf.high", "nobs", "level")
	if (isTRUE(decision)) {
		return(pars)
	} else {
		pars[[1]] <- exp(pars[[1]])
		pars[[2]] <- exp(pars[[2]])
		pars[[3]] <- exp(pars[[3]])
		return(pars)
	}

}


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

	if (class(x)[1] == "mdls") {
		pe <- possible_tidy(x)
		nms <-
			vec_data(x)[-1] |>
			{
				\(.x) {
					.y <- field(.x$fmls, "formulas")
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

	if (class(x)[1] == "mdls") {
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


# Modify md_tbl Inputs and Outputs ----------------------------------------------

#' Hammers the ellipsis arguments into a flattened list
#' @keywords internal
#' @noRd
hammer <- function(...) {

	object <- list(...)
	nms <- sapply(substitute(list(...))[-1], deparse)
	nms[names(object) != ""] <- names(object)[names(object) != ""]

	stopifnot(inherits(object, "list"))
	contents <- sapply(object, function(.x) { class(.x)[1] })
	if (!any(
		contents %in% c(
			"formula",
			"fmls",
			"mdls",
			arcana:::template_models
		)
	)) {
		message("Every object entered is not appropriate for the `md_tbl`.")
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
		} else if (class(x)[1] == "fmls") {
			# Fmls archetypes
			m <- append(m, as.list(x))
		} else if (class(x)[1] == "mdls") {
			# Model archetypes
			m <- append(m, as.list(x))
		} else if (class(x)[1] %in% arcana:::template_models) {
			# Standard modeling objects
			z <- mx(x)
			m <- append(m, as.list(z))
		} else {
			# For unknown objects (would include a warning)
			m <- append(m, as.list(x))
		}

		# Creating new names for everything but model archetypes
		n <- length(m)
		nm <- paste0(nms[i], "_", 1:n)

		if (length(m) > 1) {
			names(m) <- nm
		} else {
			names(m) <- nms[i]
		}

		mtl <- append(mtl, m)
	}

	# Return completed/reshaped list
	mtl

}

#' Temper objects from the md_tbl
#'
#' @param object A model `md_tbl` object
#' @export
temper <- function(object, ...) {

	# Validate
	stopifnot(inherits(object, "md_tbl"))

	# Trim away unnecessary variables
	# Unnest the parameter estimates and model information
	raw <-
		object |>
		subset(run == TRUE) |>
		subset(
			select = c(
				type,
				subtype,
				name,
				number,
				formula,
				outcome,
				exposure,
				mediator,
				interaction,
				strata,
				level,
				parameter_estimates,
				model_info,
				terms
			)
		) |>
		dplyr::mutate(parameter_estimates = purrr::map(parameter_estimates, tibble::as_tibble)) |>
		tidyr::unnest(parameter_estimates) |>
		dplyr::mutate(model_info = purrr::map(model_info, tibble::as_tibble)) |>
		tidyr::unnest(model_info, names_repair = "minimal")

	nms <- colnames(raw)
	nms[duplicated(nms)] <- paste0("global.", nms[duplicated(nms)])
	colnames(raw) <- nms

	# Return renamed raw data
	raw

}


