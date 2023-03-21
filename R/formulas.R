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
fmls <- function(x = unspecified(),
								 role = list(),
								 label = list(),
								 group = list(),
								 pattern = character(),
								 ...) {

	# Break early if nothing is given
	# If appropriate class, but empty, then also break early but warn/message
	if (length(x) == 0) {
		return(new_fmls())
	}

	# Wrap class of instructions in a list if not already a formula
	if (class(role) == "formula") {
		role <- list(role)
	}
	if (class(label) == "formula") {
		label <- list(label)
	}
	if (class(group) == "formula") {
		group <- list(group)
	}

	# Obtain main component for dispatching, will always be a `tm` object
	# Update the terms with new attributes, etc
	tms <-
		x |>
		tm() |>
		update(role = role,
					 label = label,
					 group = group)

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

	# Get complexity of the formula
	order <- complexity(tms)

	# Return
	new_fmls(
		termList = tmls(tms),
		pattern = pattern,
		order = order
	)
}

#' Formula vector
#' @keywords internal
#' @noRd
new_fmls <- function(termList = tmls(),
										 pattern = character(),
										 order = integer()) {

	# Validation
	vec_assert(termList, ptype = tmls())
	vec_assert(pattern, ptype = character())
	vec_assert(order, ptype = integer())

	# Everything needs to be the same length
	# This builds an atomic/vectorized object on top of `list_of` constructor
	new_rcrd(
		fields = list(
			"terms" = termList,
			"pattern" = pattern,
			"order" = order
		),
		class = "fmls"
	)
}

#' @rdname fmls
#' @export
is_fmls <- function(x) {
	inherits(x, "fmls")
}

#' @export
format.fmls <- function(x, ...) {

	# Character representation of formula
	if (vec_size(x) == 0) {
		return()
	} else {
		fmt <-
			sapply(
				x,
				function(.x) {
					# Get term list, lenght of one, into terms
					.t <- tm(.x)
					dt <- vec_data(.t)

					# Convert into formula pattern
					.l <-
						vec_restore(dt[dt$side == "left",], to = tm()) |>
						format() |>
						paste0(collapse = " + ")
					.r <-
						vec_restore(dt[dt$side == "right" | dt$side == "unknown",], to = tm()) |>
						format() |>
						paste0(collapse = " + ")

					.f <- paste(.l, sep = " ~ ", .r)
				}
			)
	}

	# Return
	fmt
}

#' @export
obj_print_data.fmls <- function(x, ...) {

	# Colorful printing
	if (vec_size(x) == 0) {
		fmt <- new_fmls()
	} else if (length(x) > 1) {
		cat(format(x), sep = "\n")
	} else if (length(x) == 1) {
		cat(format(x))
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

#' @export
methods::setOldClass(c("tm", "vctrs_rcrd"))

### Coercion methods -----------------------------------------------------------

# SELF

#' @export
vec_ptype2.fmls.fmls <- function(x, y, ...) {
	x
}

#' @export
vec_cast.fmls.fmls <- function(x, to, ...) {
	x
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
	x |>
		tm() |>
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
	x |>
		tm() |>
		stats::as.formula(env = .GlobalEnv)
}

#' @importFrom generics augment
#' @export
generics::augment

#' Augment a formula with information from a `fmls` object
#' @export
augment.fmls <- function(x, ...) {
	x
}
