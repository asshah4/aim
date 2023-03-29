# nocov start

#' Convert terms into a series of related formulas
#' @param x
#' @param simplify If the formula or terms that are supplied are complex, they
#'   may require simplification prior to being run or fit. The default `TRUE`
#'   ensures this.
#'
#' @param pattern A method for formula expansion
#' @export
fmls2 <- function(x = tm(), simplify = "all", pattern = "direct") {

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

	# Simplification of outcomes and exposures
	if (length(out) > 0 & length(exp) > 0) {
		tbl <- tidyr::expand_grid(out, exp)
	} else if (length(out) > 0 & length(exp) == 0) {
		tbl <- tidyr::expand_grid(out)
	} else if (length(out) == 0 & length(exp) > 0) {
		tbl <- tidyr::expand_grid(exp)
	} else if (length(out) == 0 & length(exp) == 0) {
		tbl <- tidyr::expand_grid()
	}

	# Add covariates if they exist
	cov <- c(prd, con, int, sta)
	if (length(cov) > 0) {
		n <- ncol(tbl)
		for (i in seq_along(cov)) {
			tbl[[n + i]] <- cov[i]
		}
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
		m1 <- tidyr::expand_grid(tbl, med)

		# mediator ~ exposure as new row
		# 	No other variables allowed
		m2 <- tidyr::expand_grid(med, exp)

		# outcome ~ mediator + exposure
		m3 <- tidyr::expand_grid(out, med, exp)

		# Bind all the tables together
		tbl <-
			m1 |>
			dplyr::bind_rows(m2) |>
			dplyr::bind_rows(m3)

	}

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
