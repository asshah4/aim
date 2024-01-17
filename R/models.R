# Class ------------------------------------------------------------------------

#' Model prototypes
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param x model object or representation
#'
#' @param formulas formula given as either an `formula` or as a `fmls` object
#'
#' @param parameter_estimates <data.frame> that contains columns
#'   representing terms and individual estimates or coefficients, can be
#'   accompanied by additional statistic columns. By default, assumes
#'
#'  * __term__ = term name
#'
#'  * __estimate__ = estimate or coefficient
#'
#' @param summary_info <list> that contains columns representing
#'   summary statistic of a model. By default, assumes...
#'
#' * __nobs__ = number of observations
#'
#' * __degrees_freedom__ = degrees of freedom
#'
#' * __statistic__ = test statistic
#'
#' * __p_value__ = p-value for overall model
#'
#' * __var_cov__ = variance-covariance matrix for predicted coefficients
#'
#' @param data_name string representing name of dataset that was used
#'
#' @param strata_variable string of a term that served as a stratifying
#'   variable
#'
#' @param strata_level value of the level of the term specified by
#'   `strata_variable`
#'
#' @name models
#' @export
mdl <- function(x = unspecified(), ...) {

	# Early break for empty objects
	if (length(x) == 0) {
		return(new_model())
	}

	UseMethod("mdl", object = x)
}

#' @rdname models
#' @export
mdl.character <- function(x,
													formulas,
													parameter_estimates = data.frame(),
													summary_info = list(),
													data_name,
													strata_variable = NA_character_,
													strata_level = NA_character_,
													...) {

	# Is the specified model type/call currently accepted?
	checkmate::assert_subset(x, .models)

	# Ensure equal length objects for the data frames
	if (length(parameter_estimates) == 0) {
		parameter_estimates <- tibble::tibble(
			term = NA_character_,
			estimate = NA
		)
	}

	if (length(summary_info) == 0) {
		summary_info <- list(
			nobs = NA,
			p_value = NA,
			statistic = NA,
			degrees_freedom = NA_integer_,
			var_cov = NA
		)
	}

	# Data arguments
	dtArgs <-
		list(dataName = data_name,
				 strataVariable = strata_variable,
				 strataLevel = strata_level)

	# Assume additional arguments are for the model (from the dots)
	dots <- list(...)

	new_model(
		modelCall = x,
		modelFormula = formulas,
		modelArgs = dots,
		parameterEstimates = parameter_estimates,
		summaryInfo = summary_info,
		dataArgs = dtArgs
	)

}

#' @rdname models
#' @export
mdl.lm <- function(x = unspecified(),
									 formulas = fmls(),
									 data_name = character(),
									 strata_variable = character(),
									 strata_level = character(),
									 ...) {

	# Class check
	checkmate::assert_class(formulas, "fmls")
	checkmate::assert_class(data_name, "character")
	checkmate::assert_class(strata_variable, "character")

	# Model class/type
	cl <- x$call
	mc <- class(x)[1]

	# Model formula
	if (length(formulas) == 0) {
		mf <-
			stats::formula(x) |>
			fmls()
	} else {
		mf <- formulas
	}

	# Model arguments
	ma <- list()
	nms <- names(cl)[!names(cl) %in% c("formula", "data", "")]
	for (i in seq_along(nms)) {
		ma[[nms[i]]] <- cl[[nms[i]]]
	}

	# Model data, if not specified
	if (length(data_name) == 0) {
		data_name <- as.character(cl[["data"]])
	}
	if (length(strata_variable) == 0 | length(strata_level) == 0) {
		strata_variable <- NA
		strata_level <- NA
	}

	da <-
		list(dataName = data_name,
				 strataVariable = strata_variable,
				 strataLevel = strata_level)


	# Get parameter information
	pe <- possible_tidy(x)

	# Get model information
	si <-
		possible_glance(x) |>
		as.list()
	si$degrees_freedom <- stats::df.residual(x)
	si$var_cov <- stats::vcov(x)

	# TODO
	# Consider warning method about empty

	# Creation
	new_model(
		modelCall = mc,
		modelFormula = mf,
		modelArgs = ma,
		parameterEstimates = pe,
		summaryInfo = si,
		dataArgs = da
	)
}

#' @rdname models
#' @export
mdl.glm <- mdl.lm

#' @rdname models
#' @export
mdl.coxph <- mdl.lm

#' @rdname models
#' @export
mdl.default <- function(x, ...) {
	stop("`mdl()` is not defined for a `",
			 class(x)[1],
			 "` object.",
			 call. = FALSE
	)
}

#' @rdname models
#' @export
model <- mdl

#' Model vector definition
#' @keywords internal
#' @noRd
new_model <- function(modelCall = character(),
											modelFormula = fmls(),
											modelArgs = list(),
											parameterEstimates = data.frame(),
											summaryInfo = list(),
											dataArgs = list()) {

	# Model description is essentially deconstructed here
	# Allows for reconstruction of the model, but lightweight, like a blueprint
	# Everything needs to be the same length

	# Arguments...
	# 	modelCall = fitting function
	# 	modelFormula = fmls() version of standard formula
	#		modelArgs = additional arguments passed to model as a named list
	#		parameterEstimates = values of terms and their estimates + statistics
	#		summaryInfo = model summary fit information, e.g. R-squared
	#		dataArgs = how and what context the model was fit

	checkmate::assert_class(modelCall, "character")
	checkmate::assert_class(modelFormula, "fmls")
	checkmate::assert_class(modelArgs, "list")
	checkmate::assert_class(parameterEstimates, "data.frame")
	checkmate::assert_class(summaryInfo, "list")
	checkmate::assert_class(dataArgs, "list")

	if (length(modelCall) == 0) {
		mc <- list()
		ma <- list()
		mf <- list()
		pe <- list()
		si <- list()
		da <- list()
	} else {
		mc <- list(modelCall)
		ma <- list(modelArgs)
		mf <- list(modelFormula)
		pe <- list(parameterEstimates)
		si <- list(summaryInfo)
		da <- list(dataArgs)
	}

	new_rcrd(
		fields = list(
			"modelCall" = mc,
			"modelFormula" = mf,
			"modelArgs" = ma,
			"parameterEstimates" = pe,
			"summaryInfo" = si,
			"dataArgs" = da
		),
		class = "mdl"
	)
}

#' @keywords internal
#' @noRd
methods::setOldClass(c("mdl", "vctrs_vctr"))

# Output -----------------------------------------------------------------------

#' @export
format.mdl <- function(x, ...) {

	# Character representation of formula
	if (vec_size(x) == 0) {
		return()
	} else {
		fmt <-
			sapply(x, FUN = function(.x) {
				f <- as.character(field(.x, "modelFormula")[[1]])
				cl <- field(.x, "modelCall")
				paste0(cl, "(", f, ")")
			})
	}
	# Return
	fmt

}

#' @export
obj_print_data.mdl <- function(x, ...) {
	if (vec_size(x) == 0) {
		new_model()
	}

	if (vec_size(x) >= 1) {
		cat(format(x), sep = "\n")
	} else {
		cat(format(x))
	}
}

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.mdl <- function(x, ...) {
	out <- format(x)
	pillar::new_pillar_shaft_simple(out, align = "left")
}

#' @export
vec_ptype_full.mdl <- function(x, ...) {
	"model"
}

#' @export
vec_ptype_abbr.mdl <- function(x, ...) {
	"mdl"
}

# Casting and coercion ---------------------------------------------------------

#' @export
vec_ptype2.mdl.mdl <- function(x, y, ...) {
	x
}

#' @export
vec_cast.mdl.mdl <- function(x, to, ...) {
	x
}
