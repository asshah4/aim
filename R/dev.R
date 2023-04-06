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

	# Simplify the complexity of key roles ----

	# Outcomes and exposures should be set as a "key pair"
	if (length(out) > 0 & length(exp) > 0) {
		tbl <- tidyr::expand_grid(outcome = out, exposure = exp)
	} else if (length(out) > 0 & length(exp) == 0) {
		tbl <- tidyr::expand_grid(outcome = out)
	} else if (length(out) == 0 & length(exp) > 0) {
		tbl <- tidyr::expand_grid(exposure = exp)
	} else if (length(out) == 0 & length(exp) == 0) {
		tbl <- tidyr::expand_grid()
	}

	# Strata terms are unique in...
	# 	Right sided variables
	# 	Not considered covariates however
	if (length(sta) > 0) {
		tidyr::expand_grid(tbl, strata = sta)
	}


	# Predictor patterns ----

	# Covariates are the fodder for pattern expansions

	switch(
		pattern,
		direct = {
		},
		sequential = {
			cov <- c(prd, con, int)

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

			cov <- c(prd, con, int)
			if (length(cov) > 0) {
				tbl <- tidyr::expand_grid(tbl, covariate = cov)
			} else {
				tbl
			}

		},
		fundamental = {

			# This forms the right hand side variables
			# However fundamental decomposition breaks the rules generally
			cov <- c(exp, prd, con, med, int, sta)
			tbl <- tidyr::expand_grid(left = out, right = cov)

		},
		message("Pattern not currently supported.")
	)


	# Mediating variables ----

	# Mediation should be done only if covariates are already added

	if (length(med) > 0 & pattern != "fundamental") {

		# Each row has been expanded for exposure and outcome
		# This will triple the number of rows

		# Mediation...
		# 	The combinations of mediation are based on causal reasoning
		# 	outcome ~ exposure + mediator + predictors
		#		mediator ~ exposure
		# 	outcome ~ mediator + exposure

		# 'outcome ~ exposure + mediator + predictors'
		# 	Covariates exists in each row already
		# 	Simply add mediator
		m1 <- tidyr::expand_grid(tbl, mediator = med)

		# 'mediator ~ exposure'
		# 	No other variables allowed
		# 	Add a new row of just this
		m2 <- tidyr::expand_grid(mediator = med, exposure = exp)

		# 'outcome ~ mediator + exposure'
		#		Only looking for effect of mediator on outcome WITH exposure
		m3 <- tidyr::expand_grid(outcome = out, mediator = med, exposure = exp)

		# Bind all the tables together
		tbl <-
			m1 |>
			dplyr::bind_rows(m2) |>
			dplyr::bind_rows(m3) |>
			unique()

	}


	# Term matrix creation ----

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
