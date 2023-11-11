#' Models with interaction variables
#'
#' @references
#' A. Figueiras, J. M. Domenech-Massons, and Carmen Cadarso, 'Regression models:
#' calculating the confidence intervals of effects in the presence of
#' interactions', Statistics in Medicine, 17, 2099-2105 (1998)
#' @keywords internal
interaction_estimates <- function(object,
																	exposure,
																	binary,
																	conf.level = 0.95,
																	present = TRUE) {

	if (!class(object) %in% .models) {
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
