### Class definition -----------------------------------------------------------

#' Vectorized formulas
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' This function defines a modified `formula` class that has been
#' vectorized. The `fmls` serves as a set of instructions or a _script_ for the
#' formula and its tm. It expands upon the functionality of formulas,
#' allowing for additional descriptions and relationships to exist between the
#' tm.
#'
#' @details
#'
#' This is not meant to supersede a [stats::formula()] object, but provide a
#' series of relationships that can be helpful in causal modeling. All `fmls`
#' can be converted to a traditional `formula` with ease. The base for this
#' object is built on the [tmls()] object.
#'
#' # Patterns
#'
#' The expansion pattern allows for instructions on how the covariates should be
#' included in different formulas. Below, assuming that _x1_, _x2_, and _x3_ are
#' covariates...
#'
#' \deqn{y = x1 + x2 + x3}
#'
#' __Direct__:
#'
#' \deqn{y = x1 + x2 + x3}
#'
#' __Seqential__:
#'
#' \deqn{y = x1}
#' \deqn{y = x1 + x2}
#' \deqn{y = x1 + x2 + x3}
#'
#' __Parallel__:
#'
#' \deqn{y = x1}
#' \deqn{y = x2}
#' \deqn{y = x3}
#'
#' @inheritSection tm Roles
#' @inheritSection tm Pluralized Arguments
#'
#' @inheritParams tm
#'
#' @param x Objects of the following types can be used as inputs
#'
#'   * `tm`
#'
#'   * `formula`
#'
#' @param pattern This is the expansion pattern used to decide how the
#'   covariates will incorporated into the formulas. The options are
#'   `c("direct", "sequential", "parallel")`. See the details for further
#'   explanation.
#'
#'   * __direct__: the covariates will all be included in each formula
#'
#'   * __sequential__: the covariates will be added sequentially, one by one, or
#'   by groups, as indicated
#'
#'   * __parallel__: the covariates or groups of covariates will be placed in
#'   parallel
#'
#' @param ... Arguments to be passed to or from other methods
#'
#' @return An object of class `fmls`
#' @name fmls
#' @export
fmls <- function(x = tm(),
								 simplify = TRUE,
								 pattern = c("direct",
								 						"sequential",
								 						"parallel",
								 						"fundamental"),
								 ...) {


	# Break early if nothing is given
	# If appropriate class, but empty, then also break early but warn/message
	if (length(x) == 0) {
		stop("An object must be given to be converted by `fmls()`")
	}

	# Convert to term object if possible
	validate_class(x, c("tm", "formula"))
	if (inherits(x, "formula")) {
		x <- tm(x)
	}

	# Check pattern
	if (length(pattern) > 1) {
		pattern <- pattern[1] # Direct
	}
	if (!pattern %in% .patterns) {
		stop("The pattern ",
				 deparse(pattern),
				 " is not yet supported.",
				 call. = FALSE)
	}

	# Take terms and create a matrix
	# A <fmls> object is a group of terms that can or have been expanded
	# <tm> object serves as a key to relay back information about individual terms
	tmKey <- vec_proxy(x)

	# Formula shape is based off of roles primarily
	# 	Take terms and crystallize them into a matrix
	# 	Each column is a term name, and each row is a defined role
	nms <- tmKey$term

	# Handle complexity
	out <- tmKey$term[tmKey$role == "outcome"]
	exp <- tmKey$term[tmKey$role == "exposure"]
	prd <- tmKey$term[tmKey$role == "predictor"]
	con <- tmKey$term[tmKey$role == "confounder"]
	med <- tmKey$term[tmKey$role == "mediator"]
	int <- tmKey$term[tmKey$role == "interaction"]
	sta <- tmKey$term[tmKey$role == "strata"]

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
			cov <- c(prd, con, int)

			if (length(cov) > 0) {
				for (i in seq_along(cov)) {
					tbl <-
						tidyr::expand_grid(tbl, "{paste0('covariate_', i)}" := cov[i])
				}
			}
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
				tbl <- tidyr::expand_grid(tbl, covariates = cov)
			} else {
				tbl
			}

		},
		fundamental = {

			# This forms the right hand side variables
			# However fundamental decomposition breaks the rules generally
			cov <- c(exp, prd, con, med, int, sta)
			tbl <- tidyr::expand_grid(left = out, right = cov)
			message_fundamental_pattern(med, sta)

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

	# Return term matrix ----

	# Create a list where each item is a vector of terms
	# These can be then turned into a term matrix
	tmMat <-
		lapply(as.list(as.data.frame(t(tbl))), function(.x) {
			table(.x) |>
				rbind() |>
				as.data.frame()
		}) |>
		dplyr::bind_rows() |>
		mutate(across(everything(), ~ dplyr::if_else(is.na(.x), 0, .x)))


	new_fmls(termMatrix = tmMat,
						key = x)

}


new_fmls <- function(termMatrix = data.frame(),
											key = tm()) {

	new_rcrd(
		fields = termMatrix,
		key = key,
		class = "fmls"
	)

}

#' @export
format.fmls <- function(x, ...) {

	# Break into matrix and key
	tmMat <- vec_data(x)
	key <- vec_data(attr(x, "key"))

	fmt <-
		apply(
			tmMat,
			MARGIN = 1,
			FUN = function(.x) {
				.y <- key[key$term %in% names(.x[which(.x == 1)]),]

				if ("mediator" %in% .y$role & !("outcome" %in% .y$role)) {
					# Handle mediation formula
					.l <-
						vec_restore(.y[.y$role == "mediator", ], to = tm()) |>
						format() |>
						paste0(collapse = " + ")

					.r <-
						vec_restore(.y[.y$side == "right" &
													 	.y$role != "mediator", ], to = tm()) |>
						format() |>
						paste0(collapse = " + ")

					.f <- paste(.l, sep = " ~ ", .r)
				} else {
					.l <-
						vec_restore(.y[.y$side == "left", ], to = tm()) |>
						format() |>
						paste0(collapse = " + ")

					.r <-
						vec_restore(.y[.y$side == "right", ], to = tm()) |>
						format() |>
						paste0(collapse = " + ")

					.f <- paste(.l, sep = " ~ ", .r)

				.f
			}
		})

	# Return
	fmt
}

#' @export
obj_print_data.fmls <- function(x, ...) {

	# Colorful printing
	if (length(x) > 1) {
		cat(format(x), sep = "\n")
	} else if (length(x) == 1) {
		cat(format(x))
	}
}

#' Formula vector
#' @keywords internal
#' @noRd

#' @rdname fmls
#' @export
is_fmls <- function(x) {
	inherits(x, "fmls")
}

#' @export
vec_ptype_full.fmls <- function(x, ...) {
	"formulas"
}

#' @export
vec_ptype_abbr.fmls <- function(x, ...) {
	"fmls"
}

#' @export
methods::setOldClass(c("fmls", "vctrs_rcrd"))

### Coercion methods -----------------------------------------------------------

# SELF

#' @export
vec_ptype2.fmls.fmls <- function(x, y, ...) {
	x
}

#' @export
vec_cast.fmls.fmls <- function(x, to, ...) {

	# Take left hand side as original
	# Standardize when possible to the LHS

	# Terms are handled first
	xkey <- attr(x, "key")
	ykey <- attr(y, "key")

	tmat <-
		rbind(vec_proxy(xkey), vec_proxy(ykey)) |>
		unique()

	dups <- duplicated(tmat$term)

	if (any(dups)) {
		message("Duplicated terms will be discarded, with newest added terms being the first to be removed")
	}

	newKey <-
		tmat[!dups, ] |>
		vec_restore(to = tm())

	# Formulas are handled second
	xmat <- vec_proxy(x)
	ymat <- vec_proxy(y)
	fmat <-
		rbind(xmat, ymat) |>
		unique()

	new_fmls(termMatrix = fmat, key = newKey)
}

# CHARACTER

#' @export
vec_ptype2.fmls.character <- function(x, y, ...) y # X = fmls

#' @export
vec_ptype2.character.fmls <- function(x, y, ...) x # X = character

#' @export
vec_cast.fmls.character <- function(x, to, ...) {
	# order is flipped, such that `x` is character
	# Cast from character into fmls
	x
}

#' @export
vec_cast.character.fmls <- function(x, to, ...) {
	# order is flipped, such that `x` is fmls
	x |>
		stats::formula(env = .GlobalEnv) |>
		deparse1()
}


# FORMULA

#' @export
vec_ptype2.fmls.formula <- function(x, y, ...) {
	x
}

#' @export
vec_ptype2.formula.fmls <- function(x, y, ...) {
	y
}

#' @export
vec_cast.formula.fmls <- function(x, to, ...) {
	# Cast from `fmls` into `formula`
	attr(x, "key") |>
		stats::as.formula(env = .GlobalEnv)
}

#' @export
vec_cast.fmls.formula <- function(x, to, ...) {
	# Cast from `formula` into `fmls`
	fmls(x)
}

### Formula Helpers ------------------------------------------------------------

#' @export
formula.fmls <- function(x, ...) {
	attr(x, "key") |>
		stats::as.formula(env = .GlobalEnv)
}
