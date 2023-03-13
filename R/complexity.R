#' Identify complexity of a formula
#'
#' A `formula` or `fmls` object may have several levels of complexity that need
#' to be assessed. This affects how the formula may need to be expanded based on
#' specific or special terms, such as a mediation, interaction, etc. It as
#' designed for usage with the built-in `fmls` class and its corresponding `tm`
#' objects, however, can be used for generic formulas if needed as well.
#'
#' @details
#'
#' # Rules of Roles
#'
#' Roles may be given or assumed based on the formula definition, such as LHS
#' objects being outcomes and RHS objects being predictors, particularly in a
#' model-focused paradigm. This makes certain assumptions about the prescribed
#' roles that a term can be given:
#'
#' 1. There can only be a single `outcome` term on the _LHS_.
#'
#' 1. The _RHS_ can not contain `outcome` terms.
#'
#' 1. There can only be a single `exposure` term on the _RHS_.
#'
#' 1. `interaction` terms must be expanded against an `exposure`, such that both
#' terms are present independently and as an interaction e.g. `Y ~ X + I + X:I`
#'
#' 1. There may not be &gt; 1 `mediator` on the _RHS_
#'
#' 1. `mediator` terms may not exist alone on the _RHS_.
#'
#' 1. `mediator` terms on the _RHS_ exist along with the corresponding
#' `exposure`, which may confound the relationship between `mediator` and
#' `outcome`.
#'
#' 1. `mediator` terms may exist on the _LHS_, but the `exposure` term must be
#' on the _RHS_ without other `confounder` terms.
#'
#' 1. A single _meta_ term, such as `strata`, may only exist on the _RHS_.
#'
#' # Complexity
#'
#' The complexity of a formula is derived from how well it follows the _rules of
#' roles_, how informative the _directionality_ of the terms are, and the number
#' of terms on the _LHS_ and _RHS_
#'
#' | Order | Rules | Directionality | LHS | RHS |
#' | :-- | :-: | :-: | --: | --: |
#' | 0th | + | - | &le; 1 | &le; 1 |
#' | 1st | - | - | 1 | 1 |
#' | 2nd | + | + | 1 | &ge; 1 |
#' | 3rd | - | - | 1 | &ge; 1 |
#' | 4th | - | + | &gt; 1 | &ge; 1 |
#'
#' @param x An object that is either a single `formula`, `fmls` or vector of `tm` objects
#' @seealso [tm()] for review of roles that can be defined.
#' @return A single integery value from `0:5`
#' @export
complexity <- function(x) {

	# Require a group of `tm` objects eventually, but may start with formulas/fmls
	validate_class(x, c("formula", "fmls", "tmls", "tm"))

	# Can only be a single formula or term list
	if (inherits(x, c("formula", "fmls", "tmls")) & length(x) > 1) {
		paste0("The argument `", deparse(substitute(x)), "` has length > 1")
	}

	# Convert to terms
	cl <- class(x)[1]

	if (cl == "tmls") {
		t <- x[[1]]
	} else if (cl == "fmls") {
		t <- field(x, "formula")[[1]]
	} else if (cl == "formula") {
		t <- tm(x)
	} else if (cl == "tm") {
		t <- x
	}

	stopifnot("Requires the argument can be converted to a `tm` object" =
							inherits(t, "tm"))

	# Order
	order <- integer()

	# Zeroeth
	# 	-> only single term object

	# First
	# 	Does not follow rules of roles
	# 	lhs = 1
	# 	rhs = 1
	# 	strata = 0

	# Second
	# 	Follows rules of roles
	# 	lhs = 1
	# 	rhs = exposure + confounder
	# 	rhs = mediator + exposure + confounder
	# 	rhs =/= outcome
	# 	rhs = exposure + interaction + exposure:interaction
	# 	strata = 1
	# 	interaction >= 0

	# Third
	# 	Does not follow rules of roles
	# 	lhs = 1
	# 	rhs > 1 exposure
	# 	rhs > 1 mediator
	# 	rhs = exposure + mediator
	# 	strata > 1

	# Fourth
	# 	lhs > 1

	outcome <- components(t, role = "outcome")
	predictor <- components(t, role = "predictor")
	exposure <- components(t, role = "exposure")
	confounder <- components(t, role = "confounder")
	mediator <- components(t, role = "mediator")
	interaction <- components(t, role = "interaction")
	strata <- components(t, role = "strata")

	# number of variables
	out <- length(unique(outcome))
	exp <- length(exposure)
	prd <- length(c(confounder, predictor))
	med <- length(mediator)
	sta <- length(strata)
	int <- length(interaction)

	# number of left and right runes
	left <- sum(out)
	right <- sum(exp, prd, med, int)
	n <- sum(left, right)

	# Zeroeth order
	if (length(t) == 1) {
		order <- 0L
	}

	# First order
	if (n == 2) {
		order <- 1L
	}

	# Second order
	if (length(t) >= 2 & sta <= 1) {
		if (out == 1 & any(exp) & med == 0) {
			order <- 2L
		}
		if (out == 0 & med == 1 & exp == 1) {
			order <- 2L
		}
		if (out == 1 & med == 1 & exp == 0 & prd == 0) {
			order <- 2L
		}
		if (out == 1 & prd > 1 & exp == 0 & med == 0) {
			order <- 2L
		}
		if (out == 1 & prd > 1 & exp == 1 & med == 0) {
			order <- 2L
		}
		if (sta == 1 & exp == 1) {
			order <- 2L
		}
		# drop the interaction term capacity if its only a second order formula
		if (int > 0 &  prd > 0 & out == 1) {
			order <- 2L
		}
	}

	# Third order
	if (length(t) > 2) {
		if (all(out, exp, med)) {
			order <- 3L
		}
		if (exp > 1) {
			order <- 3L
		}
		if (med > 1) {
			order <- 3L
		}
		if (sta > 1) {
			order <- 3L
		}
	}

	# Fourth order
	if (left > 1) {
		order <- 4L
	}

	# return
	order
}

#' Simplify a complex formula
#'
#'
#' @details
#'
#' Takes either a `tm`, `fmls`, object or a `formula` object as its main
#' argument. If a `fmls` object is given, then the attributes/properties of
#' those objects are maintained.
#'
#' @param x A `tm`, `fmls`, or `formula` object
#' @param ... For future extensions
#' @return A vector of `fmls` objects
#' @name simplify
#' @export
simplify <- function(x, ...) {

	# Validate
	validate_class(x, c("fmls", "formula"))
	stopifnot("Must be applied to a single formula at a time."
						= length(x) == 1)

	# Ensure appropriate type
	f <- fmls(x)
	o <- field(x, "order")
	t <- field(f, "formula")[[1]]

	# The number of formulas that will be multiplicative by complexity
	# 	N = n x outcome
	#		N = n x exposure
	#		N = n x mediator x 3

	# Working formula list
	workingList <-
		x |>
		simplify_outcomes() |>
		simplify_exposures() |>
		simplify_mediation()

	# Return
	unique(workingList)

}


#' @rdname simplify
#' @export
simplify_outcomes <- function(x) {

	validate_class(x, c("fmls", "formula"))

	# If multiple fmls, need to run through them all
	f <- fmls()
	for (i in seq_along(x)) {

		t <- tm(x[i])
		out <- components(t, role = "outcome")
		exp <- components(t, role = "exposure")
		prd <- components(t, role = "predictor")
		con <- components(t, role = "confounder")
		med <- components(t, role = "mediator")
		int <- components(t, role = "interaction")
		sta <- components(t, role = "strata")

		# Simplify RHS
		cov <- c(exp, prd, con, med, int, sta)

		for (j in seq_along(out)) {

			o <- c(out[j], cov) |>
				fmls()

			f <- c(f, o)

		}
	}

	# Return
	f

}

#' @rdname simplify
#' @export
simplify_exposures <- function(x) {

	validate_class(x, c("fmls", "formula"))

	# If multiple fmls, need to run through them all
	f <- fmls()
	for (i in seq_along(x)) {

		t <- tm(x[i])
		out <- components(t, role = "outcome")
		exp <- components(t, role = "exposure")
		prd <- components(t, role = "predictor")
		con <- components(t, role = "confounder")
		med <- components(t, role = "mediator")
		int <- components(t, role = "interaction")
		sta <- components(t, role = "strata")

		# Simplify RHS
		cov <- c(prd, con, med, int, sta)

		for (j in seq_along(exp)) {

			e <- c(out, exp[j], cov) |>
				fmls()

			f <- c(f, e)

		}
	}

	# Return
	f

}

#' @rdname simplify
#' @export
simplify_mediation <- function(x) {

	validate_class(x, c("fmls", "formula"))

	# If multiple fmls, need to run through them all
	f <- fmls()
	for (i in seq_along(x)) {

		t <- tm(x[i])
		out <- components(t, role = "outcome")
		exp <- components(t, role = "exposure")
		prd <- components(t, role = "predictor")
		con <- components(t, role = "confounder")
		med <- components(t, role = "mediator")
		int <- components(t, role = "interaction")
		sta <- components(t, role = "strata")

		# Simplify RHS
		cov <- c(prd, con, int, sta)

		# Mediation...
		# 	The combinations of mediation are based on causal reasoning
		# 	outcome ~ exposure + mediator + predictors
		#		mediator ~ exposure
		# 	outcome ~ mediator

		for (j in seq_along(med)) {

			# outcome ~ exposure
			m1 <-
				c(out, exp, cov) |>
				fmls()

			# mediator ~ exposure
			md <- vec_proxy(c(med[j], exp))
			md$side[md$term == as.character(med[j])] <- "left"
			m2 <-
				vec_restore(md, to = tm()) |>
				fmls()

			# outcome ~ mediator + exposure
			m3 <-
				c(out, med[j], exp) |>
				fmls()

			f <- c(f, m1, m2, m3)
		}
	}

	# Return
	f

}

temp_function <- function() {
	# Validation, also can take more than one spell at a time
	validate_class(s, "spell")
	sl <- s

	for (i in seq_along(sl)) {
		t <- field(sl[i], "runes")[[1]]
		order <- decipher(t)
		p <- field(sl[i], "pattern")

		# roles
		rls <- roles(t)
		labs <- labels(t)
		outcome <- names(rls[rls == "outcome"])
		predictor <- names(rls[rls == "predictor"])
		exposure <- names(rls[rls == "exposure"])
		confounder <- names(rls[rls == "confounder"])
		mediator <- names(rls[rls == "mediator"])
		interaction <- names(rls[rls == "interaction"])
		strata <- names(rls[rls == "strata"])

		if (length(interaction) > 0 & length(exposure) > 0) {
			combined <-
				paste(rep(exposure, each = length(interaction)),
							interaction,
							sep = ":")
		} else {
			combined <- character()
		}

		covariates <- c(confounder, predictor, interaction, combined)

		#### Creating formulas one level down

		# Order = 2
		if (order == 2) {
			if (length(mediator) > 0 & length(outcome) == 0) {
				left <- mediator
				right <- setdiff(rhs(t), mediator)
			} else if (length(interaction) > 0) {
				left <- lhs(t)
				right <- c(exposure, covariates)
			} else {
				left <- lhs(t)
				right <- rhs(t)
			}

			if (p == "direct") {
				right <- paste0(right, collapse = " + ")
			}

			for (j in seq_along(left)) {
				for (k in seq_along(right)) {

					f <- paste0(left[j], " ~ ", right[k])
					if (length(strata) > 0) { for (l in seq_along(strata)) {
						mt <-
							match_runes(t, stats::formula(f)) |>
							c(get_runes(t, field = "runes", value = strata[l]))
						sl <- append(
							sl,
							new_spell(
								formula = f,
								runes = mt,
								pattern = p,
								order = decipher(mt)
							)
						)
					}} else {
						mt <- match_runes(t, stats::formula(f))
						p <- field(sl[i], "pattern")
						sl <- append(
							sl,
							new_spell(
								formula = f,
								runes = mt,
								pattern = p,
								order = decipher(mt)
							)
						)
					}
				}
			}
		}

		# Order = 3
		if (order == 3) {

			# Exposure on the right if outcome is present
			if (length(outcome) > 0) {
				for (j in seq_along(exposure)) {
					f <- paste0(
						outcome,
						" ~ ",
						paste(c(exposure[j], covariates), collapse = " + ")
					)
					if (length(strata) > 0) for (k in seq_along(strata)) {
						mt <-
							match_runes(t, stats::formula(f)) |>
							c(get_runes(t, field = "runes", value = strata[k]))
						p <- field(sl[i], "pattern")
						sl <- append(
							sl,
							new_spell(
								formula = f,
								runes = mt,
								pattern = p,
								order = decipher(mt)
							)
						)
					} else {
						mt <- match_runes(t, stats::formula(f))
						p <- field(sl[i], "pattern")
						sl <- append(
							sl,
							new_spell(
								formula = f,
								runes = mt,
								pattern = p,
								order = decipher(mt)
							)
						)
					}
				}
			}

			# mediation if present
			if (length(mediator) > 0) {
				for (j in 1:seq_along(mediator)) {
					# mediator on the right if outcome is available
					if (length(outcome) > 0) {
						f <- paste0(
							outcome,
							" ~ ",
							mediator[j]
						)
						if (length(strata) > 0) for (k in seq_along(strata)) {
							mt <-
								match_runes(t, stats::formula(f)) |>
								c(get_runes(t, field = "runes", value = strata[k]))
							p <- field(sl[i], "pattern")
							sl <- append(
								sl,
								new_spell(
									formula = f,
									runes = mt,
									pattern = p,
									order = decipher(mt)
								)
							)
						} else {
							mt <- match_runes(t, stats::formula(f))
							p <- field(sl[i], "pattern")
							sl <- append(
								sl,
								new_spell(
									formula = f,
									runes = mt,
									pattern = p,
									order = decipher(mt)
								)
							)
						}
					}

					# mediator on the left
					f <- paste0(
						mediator[j],
						" ~ ",
						paste(c(exposure, covariates), collapse = " + ")
					)
					# adding strata to the decomposition if needed
					if (length(strata) > 0) for (k in seq_along(strata)) {
						mt <-
							match_runes(t, stats::formula(f)) |>
							c(get_runes(t, field = "runes", value = strata[k]))
						p <- field(sl[i], "pattern")
						sl <- append(
							sl,
							new_spell(
								formula = f,
								runes = mt,
								pattern = p,
								order = decipher(mt)
							)
						)
					} else {
						mt <- match_runes(t, stats::formula(f))
						p <- field(sl[i], "pattern")
						sl <- append(
							sl,
							new_spell(
								formula = f,
								runes = mt,
								pattern = p,
								order = decipher(mt)
							)
						)
					}
				}
			}
		}

		# Order = 4
		if (order == 4) {
			for (j in seq_along(outcome)) {
				f <- paste0(
					outcome[j],
					" ~ ",
					paste(c(exposure, mediator, covariates), collapse = " + ")
				)
				mt <- match_runes(t, stats::formula(f))
				p <- field(sl[i], "pattern")
				sl <- append(
					sl,
					new_spell(
						formula = f,
						runes = mt,
						pattern = p,
						order = decipher(mt)
					)
				)
			}
		}
	}

	# Return spells, expected to have one level order less
	sl[field(sl, "order") > 0] |>
		unique()
}

