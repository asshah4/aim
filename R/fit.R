#' @importFrom generics fit
#' @export
generics::fit

#' @export
fit.fmls <- function(object,
										 .fn,
										 ...,
										 data,
										 .unpack = TRUE) {

	cl <- match.call()
	args <- list(...)

	# Validate functions
	.fn <- as.character(cl[[3]])
	stopifnot("The .fn argument supplied is not yet supported" =
							.fn %in% .models)

	# Check data
	stopifnot(is.data.frame(data))
	dataName <- deparse1(cl[["data"]])

	# Models to be returned
	modList <- mdl()

	for (i in seq_along(object)) {
		t <- field(object, "terms")[[1]]
		f <- stats::as.formula(t)
		sta <- filter.tm(t, role == "strata")

		# If no strata, can model simply
		if (length(sta) == 0) {
			args$data <- quote(data)
			x <- do.call(.fn, args = c(formula = f, args))
			mx <- mdl(x,
								formulas = object,
								data_name = dataName)
			modList <- c(modList, mx)
		} else {
			for (j in seq_along(sta)) {
				strata <- as.character(sta[j])
				# Ignores NA values
				strataLevels <- unique(stats::na.omit(data[[strata]]))
				for (k in seq_along(strataLevels)) {
					strataInfo <- list()
					strataInfo[[strata]] <- strataLevels[k]
					strataData <- data[data[[strata]] == strataLevels[k], ]
					args$data <- quote(strataData)
					x <- do.call(.fn, args = c(formula = f, args))
					mx <- mdl(x,
										formulas = object,
										data_name = dataName,
										strata_info = strataInfo)
					modList <- c(modList, mx)
				}
			}
		}
	}

	# Return
	if (.unpack) {
		field(modList, "model")
	} else {
		modList
	}

}

#' @importFrom generics tidy
#' @export
generics::tidy

#' @export
tidy.mdl <- function(x,
											conf.int = TRUE,
											conf.level = 0.95,
											exponentiate = TRUE,
											...) {
	# Get lists of models
	lom <- vec_data(x)$model

	purrr::map(
		lom,
		~ possible_tidy(
			.x,
			conf.int = conf.int,
			conf.level = conf.level,
			exponentiate = exponentiate
		)
	)
}

#' Create a "fail-safe" of tidying fits
#' @noRd
my_tidy <- function(x,
										conf.int = TRUE,
										conf.level = 0.95,
										exponentiate = TRUE,
										...) {
	broom::tidy(
		x,
		conf.int = conf.int,
		conf.level = conf.level,
		exponentiate = exponentiate
	)
}

#' Local load of it if not when package starts
#' @noRd
possible_tidy <-
	purrr::possibly(my_tidy, otherwise = NA, quiet = FALSE)

#' @importFrom generics glance
#' @export
generics::glance

#' @export
glance.mdls <- function(x, ...) {
	lom <- vec_data(x)$model
	purrr::map(lom, ~ possible_glance(.x, ))
}

#' Create a "fail-safe" of glance at fits
#' @noRd
my_glance <- function(x, ...) {
	broom::glance(x)
}

#' Local load of it if not when package starts
#' @noRd
possible_glance <-
	purrr::possibly(my_glance, otherwise = NA, quiet = FALSE)
