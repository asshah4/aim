#' Expand formula by a pattern
#' Must occur after the formula has been simplified
#' @return `fmls` objects expanded by pattern
#' @name patterns
#' @export
pattern <- function(x, ...) {

	# Validation, also can take more than one spell at a time
	validate_class(x, c("fmls", "formula"))

	formulaList <- fmls()
	for (i in seq_along(x)) {

		patternField <- field(x[i], "pattern")
		switch(patternField,
					 direct = {
					 	formulaList <- c(formulaList, pattern_direct(x[i]))
					 },
					 sequential = {
					 	formulaList <- c(formulaList, pattern_sequential(x[i]))
					 },
					 parallel = {
					 	formulaList <- c(formulaList, pattern_parallel(x[i]))
					 },
					 fundamental = {
					 	formulaList <- c(formulaList, pattern_fundamental(x[i]))
					 },
					 message("Pattern not currently supported."))
	}

	# Return
	unique(formulaList)
}


#' @rdname patterns
#' @export
pattern_sequential <- function(x, ...) {

	validate_class(x, c("fmls", "formula"))
  if (inherits(x, "formula")) {
  	message_formula_to_fmls()
    x <- fmls(x, pattern = "sequential")
  }

	# If multiple fmls, need to run through them all
	f <- fmls()
	for (i in seq_along(x)) {

		t <- tm(x[i])
		out <- filter.tm(t, role == "outcome")
		exp <- filter.tm(t, role == "exposure")
		prd <- filter.tm(t, role == "predictor")
		con <- filter.tm(t, role == "confounder")
		med <- filter.tm(t, role == "mediator")
		int <- filter.tm(t, role == "interaction")
		sta <- filter.tm(t, role == "strata")

		# Determine the covariates
		cov <- c(prd, con, int)

		# Left and right
		allLeft <- filter.tm(t, side == "left")
		allRight <- filter.tm(t, side == "right")

		# Fixed right side would include exposures, strata, or grouped variables
		fixedRight <- allRight[!vec_in(allRight, cov)]

		# Sequential expansion based on if there are exposure variables or strata
		n <- ifelse(length(fixedRight) == 0 & length(cov) > 0, 1, 0)

		for (j in n:length(cov)) {
			rightSide <- c(fixedRight, cov[0:j])
			f <- c(f, fmls(c(allLeft, rightSide)))
		}
	}

	# Return
	f
}

#' @rdname patterns
#' @export
pattern_parallel <- function(x, ...) {

	validate_class(x, c("fmls", "formula"))
  if (inherits(x, "formula")) {
  	message_formula_to_fmls()
    x <- fmls(x, pattern = "parallel")
  }

	# If multiple fmls, need to run through them all
	f <- fmls()
	for (i in seq_along(x)) {

		t <- tm(x[i])
		out <- filter.tm(t, role == "outcome")
		exp <- filter.tm(t, role == "exposure")
		prd <- filter.tm(t, role == "predictor")
		con <- filter.tm(t, role == "confounder")
		med <- filter.tm(t, role == "mediator")
		int <- filter.tm(t, role == "interaction")
		sta <- filter.tm(t, role == "strata")

		# Determine the covariates
		# Okay to split apart interaction and mediation terms
		cov <- c(prd, con)

		# Left and right
		allLeft <- filter.tm(t, side == "left")
		allRight <- filter.tm(t, side == "right")

		# Fixed right side would include exposures, strata, or grouped variables
		fixedRight <- allRight[!vec_in(allRight, cov)]

		# Parallel expansion based on number of covariates
		if (length(cov) == 0) {
			n <- 1
		} else {
			n <- seq_along(cov)
		}

		for (j in n) {
			rightSide <- c(fixedRight, cov[j])
			f <- c(f, fmls(c(allLeft, rightSide)))
		}

	}

	# Return
	f
}

#' @rdname patterns
#' @export
pattern_direct <- function(x, ...) {

	validate_class(x, c("fmls", "formula"))
  if (inherits(x, "formula")) {
  	message_formula_to_fmls()
    x <- fmls(x, pattern = "parallel")
  }

	# If multiple fmls, need to run through them all
	f <- fmls()
	for (i in seq_along(x)) {

		t <- tm(x[i])
		out <- filter.tm(t, role == "outcome")
		exp <- filter.tm(t, role == "exposure")
		prd <- filter.tm(t, role == "predictor")
		con <- filter.tm(t, role == "confounder")
		med <- filter.tm(t, role == "mediator")
		int <- filter.tm(t, role == "interaction")
		sta <- filter.tm(t, role == "strata")

		# Left and right
		left <- filter.tm(t, side == "left")
		right <- filter.tm(t, side == "right")

		f <- c(f, fmls(c(left, right)))

	}

	# Return
	f
}

#' @rdname patterns
#' @export
pattern_fundamental <- function(x, ...) {

	validate_class(x, c("fmls", "formula"))
  if (inherits(x, "formula")) {
  	message_formula_to_fmls()
    x <- fmls(x, pattern = "parallel")
  }

	# If multiple fmls, need to run through them all
	f <- fmls()
	for (i in seq_along(x)) {

		t <- tm(x[i])
		out <- filter.tm(t, role == "outcome")
		exp <- filter.tm(t, role == "exposure")
		prd <- filter.tm(t, role == "predictor")
		con <- filter.tm(t, role == "confounder")
		med <- filter.tm(t, role == "mediator")
		int <- filter.tm(t, role == "interaction")
		sta <- filter.tm(t, role == "strata")

		# Right side
		right <- c(exp, prd, con, med, int, sta)

		for (j in seq_along(out)) {
			for (k in seq_along(right)) {
				f <- c(f, fmls(c(out[j], right[k])))
			}
		}

	}

	# Return
	f
}
