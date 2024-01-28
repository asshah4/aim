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
#' @return A `<data.frame>` with `n = levels(interaction)` rows (for the
#'   presence or absence of the interaction term) and `n = 5` columns:
#'
#'   - estimate: beta coefficient for the interaction effect based on level
#'
#'   - conf_low: lower bound of confidence interval for the estimate
#'
#'   - conf_high: higher bound of confidence interval for the estimate
#'
#'   - p_value: p-value for the overall interaction effect *across levels*
#'
#'   - nobs: number of observations within the interaction level
#'
#'   - level: level of the interaction term
#'
#' @references
#' A. Figueiras, J. M. Domenech-Massons, and Carmen Cadarso, 'Regression models:
#' calculating the confidence intervals of effects in the presence of
#' interactions', Statistics in Medicine, 17, 2099-2105 (1998)
#'
#' @export
estimate_interaction <- function(object,
																 exposure,
																 interaction,
																 conf_level = 0.95,
																 ...) {

  # TODO
  # For development of this, would need to add some way to generalize
  # 	Categorical interaction variable levels
  # 	Number of observations in each level
  # Confidence interval estimates
  #   Simulation / bootstrapping methods

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

  # Check if data is availabe as an attribute from the model table object
  datLs <- attr(object, "dataList")
  if (length(datLs) == 0 | !object$data_id %in% names(datLs)) {
    stop("The model table object does not have the data available.")
  }

  # Interaction term and its levels in the dataset
  exp <- exposure
  int <- interaction
  it <- paste0(exp, ":", int)
  dat <- datLs[[object$data_id]]
  lvls <-  levels(factor(dat[[int]]))
  nobs <- table(dat[[int]])
  stopifnot(
    "`estimate_interaction()` currently only accepts binary interaction terms."
    = length(lvls) == 2
  )

  # Get the model(s) and corresponding data
  mod <-
    object |>
    reduce_models() |>
    dplyr::select(model_call, number, outcome, exposure, interaction, term, estimate, conf_low, conf_high, p_value, nobs, degrees_freedom, var_cov)

  pVal <- mod$p_value[mod$term == it]

  # Beta coefficients are based on the model type
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
  		conf_high = coefs[[exp]] + halfConf,
  		p_value = pVal,
  		nobs = nobs[[lvls[1]]],
  		level = lvls[[1]]
  	)

  # When interaction term is present
  halfConf <-
	  stats::qt(conf_level / 2 + 0.5, df = degFree) *
	  	sqrt(coefVar[[exp]] + coefVar[[it]] + 2 * varCovMat[exp, it])

  present <- list(
  	estimate = coefs[[exp]] + coefs[[it]],
  	conf_low = (coefs[[exp]] + coefs[[it]]) - halfConf,
  	conf_high = (coefs[[exp]] + coefs[[it]]) + halfConf,
		p_value = pVal,
		nobs = nobs[[lvls[2]]],
		level = lvls[[2]]
  )

  # Combine the binary outputs into a small table
  intEsts <-
    dplyr::bind_rows(absent, present)

  # Return
  intEsts
}

