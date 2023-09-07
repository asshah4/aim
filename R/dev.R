# nocov start

#' @rdname models
#' @export
mdl.crr <- function(x = unspecified(),
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
		dplyr::bind_cols(dataName = data_name,
										 strataVariable = strata_variable,
										 strataLevel = strata_level)


	# Get parameter information
	pe <- possible_tidy(x)

	# Get model information
	si <- possible_glance(x)

	# Warning about empty...
	if (length(mc) == 0) {
		warning_empty_models()
	}

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

# nocov end
