# nocov start

#' Convert terms into a series of related formulas
#' @param x
#' @param simplify If the formula or terms that are supplied are complex, they
#'   may require simplification prior to being run or fit. The default `TRUE`
#'   ensures this.
#'
#' @param pattern A method for formula expansion
#' @export
fmls2 <- function(x = tm(), simplify = TRUE, pattern = "direct") {

	# Take terms and create a matrix
	# A <fmls> object is a group of terms that can or have been expanded
	# <tm> object serves as a key to relay back information about individual terms
	termKey <- vec_proxy(x)

	# Formula shape is based off of roles primarily
	# 	Take terms and crystallize them into a matrix
	# 	Each column is a term name, and each row is a defined role
	nms <- termKey$term

	# Handle complexity
	out <- termKey$term[termKey$role == "outcome"]
	exp <- termKey$term[termKey$role == "exposure"]
	prd <- termKey$term[termKey$role == "predictor"]
	con <- termKey$term[termKey$role == "confounder"]
	med <- termKey$term[termKey$role == "mediator"]
	int <- termKey$term[termKey$role == "interaction"]
	sta <- termKey$term[termKey$role == "strata"]

	# Complexity ----

	# Simplification of outcomes and exposures
	if (length(out) > 0 & length(exp) > 0) {
		tbl <- tidyr::expand_grid(outcome = out, exposure = exp)
	} else if (length(out) > 0 & length(exp) == 0) {
		tbl <- tidyr::expand_grid(outcome = out)
	} else if (length(out) == 0 & length(exp) > 0) {
		tbl <- tidyr::expand_grid(exposure = exp)
	} else if (length(out) == 0 & length(exp) == 0) {
		tbl <- tidyr::expand_grid()
	}

	# Mediation
	if (length(med) > 0) {

		# Each row has been expanded for exposure and outcome
		# This will triple the number of rows

		# Mediation...
		# 	The combinations of mediation are based on causal reasoning
		# 	outcome ~ exposure + mediator + predictors
		#		mediator ~ exposure
		# 	outcome ~ mediator

		# outcome ~ exposure + covariates exists in each row already
		# 	Simply add mediator
		m1 <- tidyr::expand_grid(tbl, mediator = med)

		# mediator ~ exposure as new row
		# 	No other variables allowed
		m2 <- tidyr::expand_grid(mediator = med, exposure = exp)

		# outcome ~ mediator + exposure
		m3 <- tidyr::expand_grid(outcome = out, mediator = med, exposure = exp)

		# Bind all the tables together
		tbl <-
			m1 |>
			dplyr::bind_rows(m2) |>
			dplyr::bind_rows(m3) |>
			unique()

	}

	# Patterns ----

	# Covariates are the fodder for pattern expansions
	cov <- c(prd, con, int, sta)
	if (length(cov) > 0) {
		n <- ncol(tbl)
		for (i in seq_along(cov)) {
			tbl[[paste0("covariate_", i)]] <- cov[i]
		}
	}


	switch(
		pattern,
		direct = {
		},
		sequential = {

			for (i in seq_along(cov)) {
				tbl <-
					tidyr::expand_grid(tbl, "{paste0('covariate_', i)}" := c(NA, cov[i]))
			}

			# Remove "bad rows" that don't follow sequential rules
			n <- length(cov)

			ntbl <- list()
			for (i in seq_along(cov)) {
				# Potential columns, may not exist
				pc <- paste0("covariate_", i - 1)
				cc <- paste0("covariate_", i + 0)
				nc <- paste0("covariate_", i + 1)

				if (i == 1) {
					# First term
					# If missing, future terms cannot be present either
					ntbl[[i]] <-
						tbl |>
						dplyr::filter(is.na(!!rlang::sym(cc)) & !is.na(!!rlang::sym(nc)))

				} else if (i == n) {
					# Last term
					# If present, previous term must also be present
					ntbl[[i]] <-
						tbl |>
						dplyr::filter(!is.na(!!rlang::sym(cc)) & is.na(!!rlang::sym(pc)))

				} else {
					# All other rows
					# If variable i is empty, i...n must also be empty
					ntbl[[i]] <-
						tbl |>
						dplyr::filter(
							(!is.na(!!rlang::sym(cc)) & is.na(!!rlang::sym(pc))) |
								(is.na(!!rlang::sym(cc)) & !is.na(!!rlang::sym(nc)))
						)

				}
			}

			# Combine the bad tables together and cull them from orignal tables
			ntbl <- unique(dplyr::bind_rows(ntbl))
			tbl <- setdiff(tbl, ntbl)

		},
		parallel = {

		},
		fundamental = {

		},
		message("Pattern not currently supported.")
	)
	# Sequential
	# Create a list where each item is a vector of terms
	# These can be then turned into a term matrix
	tblList <-
		lapply(as.list(as.data.frame(t(tbl))), function(.x) {
			table(.x) |>
				rbind() |>
				data.frame()
		})

	tmMat <-
		tblList |>
		dplyr::bind_rows() |>
		mutate(across(everything(), ~ dplyr::if_else(is.na(.x), 0, .x)))


}
# nocov end
