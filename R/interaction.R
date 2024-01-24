#' Estimating interaction effect estimates
#'
#' When using categorical interaction terms in a `<mdl_tbl>` object, estimates
#' on interaction terms and their confidence intervals can be evaluated. The
#' effect of interaction on the estimates is based on the levels of interaction
#' term. The estimates and intervals can be derived through the
#' `estimate_interaction()` function. The approach is based on the method
#' described by Figueiras et al. (1998).
#'
#' @details
#' The `estimate_interaction()` requires a `<mdl_tbl>` object that is a
#' *single row in length*. Filtering the `<mdl_tbl>` should occur prior to passing
#' it to this function. Additionally, this function assumes the interaction term
#' is binary. If it is categorical, the current recommendation is to use dummy
#' variables for the corresponding levels prior to modeling.
#'
#' @return A `<data.frame>` with `n = 2` rows (for the presence or absence of
#'   the interaction term).
#'
#' @references
#' A. Figueiras, J. M. Domenech-Massons, and Carmen Cadarso, 'Regression models:
#' calculating the confidence intervals of effects in the presence of
#' interactions', Statistics in Medicine, 17, 2099-2105 (1998)
#' @export
estimate_interaction <- function(object,
																 exposure,
																 interaction,
																 conf.level = 0.95,
																 ...) {


	validate_class(object, "mdl_tbl")
	# Check that only one row is being provided from the <mdl_tbl> object
	if (nrow(object) > 1) {
		stop("The <mdl_tbl> object must be subset to single row to estimate interactions.")
	}

  # Check exposure is in model table
  if (!exposure %in% object$exposure) {
    stop("The exposure variable is not in the model set.")
  }

  # Check if interaction is in the model table
  if (!grepl(interaction, object$interaction)) {
    stop("The interaction variable is not in the model set.")
  }

  # Filtering variables
  exp <- exposure
  int <- interaction
  it <- paste0(exp, ":", int)

  # Get the model(s) and corresponding data
  mod <-
    object |>
    reduce_models() |>
    dplyr::select(model_call, number, outcome, exposure, interaction, term, estimate, conf_low, conf_high, nobs, degrees_freedom, var_cov)

  # Betas are based on number of models
  coefs <- mod$estimate
  names(coefs) <- mod$term

  # Variance-covariance matrix
  varCovMat <- unique(mod$var_cov)[[1]]
  degFree <- unique(mod$degrees_freedom)

  # When interaction term is absent
  # Taking the diagonal of the variance-covariance matrix gives `var(term)`
  # This removes the need of having the full dataset
  coefVar <- diag(varCovMat)
  halfConf <- stats::qt(conf_level / 2 + 0.5, df = degFree) * sqrt(coefVar[[exp]])

  absent <-
  	list(
  		estimate = coefs[[exp]],
  		conf_low = coefs[[exp]] - halfConf,
  		conf_high = coefs[[exp]] + halfConf
  	)

  # When interaction term is present
  halfConf <-
	  stats::qt(conf_level / 2 + 0.5, df = degFree) *
	  	sqrt(coefVar[[exp]] + coefVar[[it]] + 2 * varCovMat[exp, it])

  present <- list(
  	estimate = coefs[[exp]] + coefs[[it]],
  	conf_low = coefs[[exp]] + coefs[[it]] - halfConf,
  	conf_high = coefs[[exp]] + coefs[[it]] + halfConf
  )

  # TODO
  # For development of this, would need to add some way to generalize
  # 	Categorical interaction variable levels
  # 	Number of observations in each level

}

#' @keywords internal
old_interaction_estimates <- function(object,
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
