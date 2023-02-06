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
#'
#' @return An object of class `fmls`
#' @name fmls
#' @export
fmls <- function(x = unspecified(),
								 role = list(),
								 group = list(),
								 label = list(),
								 pattern = character(),
								 ...) {

	# Break early if nothing is given
	# If appropriate class, but empty, then also break early but warn/message
	if (length(x) == 0) {
		return(new_fmls())
	}
	validate_class(x, c("tm", "formula"))
	if (validate_empty(x)) {
		return(new_fmls())
	}

	# Check pattern
	if (length(pattern) == 0) {
		pattern <- "direct"
	}
	if (!pattern %in% .patterns) {
		stop("The pattern ",
				 deparse(pattern),
				 " is not yet supported.",
				 call. = FALSE)
	}

	# tm list (nested for field length equivalence)
	# Updated attributes/components internally
	t <-
		tm(x) |>
		set_roles(roles = formula_to_named_list(role)) |>
		set_groups(groups = formula_to_named_list(group)) |>
		set_labels(labels = formula_to_named_list(label)) |>
		unique()

	# Look at composition of tm
	order <- decipher(t)

	# Formula
	f <- deparse1(stats::formula(t))

	# Return
	new_fmls(
		terms = t,
		pattern = pattern,
		order = order
	)
}

#' Formula vector
#' @keywords internal
#' @noRd
new_fmls <- function(terms = tmls(),
										 pattern = character(),
										 order = integer()) {

	# Validation
	vec_assert(terms, ptype = tmls())
	vec_assert(pattern, ptype = character())
	vec_assert(order, ptype = integer())

	# Everything needs to be the same length
	# This builds an atomic/vectorized object on top of `list_of` constructor
	new_rcrd(
		fields = list(
			"formula" = terms,
			"pattern" = pattern,
			"order" = order
		),
		class = "fmls"
	)
}

#' @keywords internal
#' @noRd
methods::setOldClass(c("fmls", "vctrs_rcrd"))

#' @export
format.fmls <- function(x, ...) {

	# Character representation of formula
	if (vec_size(x) == 0) {
		return()
	} else {
		fmt <-
			sapply(x, FUN = function(.x) {
				field(.x, "formula") |>
					format()
			})
	}
	# Return
	fmt
}

#' @export
obj_print_data.fmls <- function(x, ...) {

	# Colorful printing
	if (vec_size(x) == 0) {
		fmt <- new_fmls()
	} else {
		fmt <-
			sapply(
				x,
				FUN = function(.x) {
					t <- field(.x, "tm")[[1]]
					f <- stats::formula(field(.x, "formula"))
					left <- match_tm(t, lhs(f))
					right <- match_tm(t, rhs(f))

					f <-
						paste(format(left), collapse = " + ") |>
						paste(paste(format(right), collapse = " + "), sep = " ~ ")

					f
				}
			)

	}

	if (length(fmt) > 1) {
		cat(format(fmt), sep = "\n")
	} else {
		cat(format(fmt))
	}
}

#' @export
vec_ptype_full.fmls <- function(x, ...) {
	"formulas"
}

#' @export
vec_ptype_abbr.fmls <- function(x, ...) {
	"fmls"
}

# Arithmetic
vec_arith.fmls <- function(op, x, y, ...) {
	UseMethod("vec_arith.fmls", y)
}

vec_arith.fmls.default <- function(op, x, y, ...) {
	stop_incompatible_op(op, x, y)
}


#' @export
vec_ptype2.fmls.fmls <- function(x, y, ...) {
	x
}

#' @export
vec_cast.fmls.fmls <- function(x, to, ...) {
	x
}

#' @export
vec_ptype2.fmls.character <- function(x, y, ...) {
	y
}

#' @export
vec_ptype2.character.fmls <- function(x, y, ...) {
	x
}

#' @export
vec_cast.character.fmls <- function(x, to, ...) {
	format(x) # Returns a character class by default
}

#' @export
vec_ptype2.fmls.tm <- function(x, y, ...) {
	y
}

#' @export
vec_ptype2.tm.fmls <- function(x, y, ...) {
	x
}

#' @export
vec_cast.tm.fmls <- function(x, to, ...) {
	tm.fmls(x)
}

### Formula Helpers ------------------------------------------------------------

#' @export
formula.fmls <- function(x, ...) {
	format(x) |>
		stats::as.formula()
}
