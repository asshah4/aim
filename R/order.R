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
#' 1. `interaction` terms must be expanded against an `exposure`, such that both
#' terms are present independently and as an interaction e.g. `Y ~ X + I + X:I`
#'
#' 1. `mediator` terms may only exist on the _RHS_ with an `exposure` term &pm; `confounder` terms, or _alone_ (but not just with other `confounder` terms)
#'
#' 1. There can only be a single `exposure` term on the _RHS_.
#'
#' 1. A single _meta_ term, such as `strata`, may only exist on the _RHS_.
#'
#' 1. There may not be &gt; 1 `mediator` on the _RHS_
#'
#' # Complexity
#'
#' The complexity of a formula is derived from how well it follows the _rules of roles_, how informative the _directionality_ of the terms are, and the number of terms on the _LHS_ and _RHS_
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
	# 	rhs = mediator (no confounders allowed)
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

	outcome <- as.character(find_terms(t, role = "outcome"))
	predictor <- as.character(find_terms(t, role = "predictor"))
	exposure <- as.character(find_terms(t, role = "exposure"))
	confounder <- as.character(find_terms(t, role = "confounder"))
	mediator <- as.character(find_terms(t, role = "mediator"))
	interaction <- as.character(find_terms(t, role = "interaction"))
	strata <- as.character(find_terms(t, role = "strata"))

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
