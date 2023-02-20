### Term class -----------------------------------------------------------------

#' Create vectorized terms
#'
#' `r lifecycle::badge('experimental')`
#'
#' A vectorized term object that allows for additional information to be carried
#' with the variable name.
#'
#' @details
#'
#' This is not meant to replace traditional [stats::terms()], but to supplement
#' it using additional information that is more informative for causal modeling.
#'
#' # Roles
#'
#' Specific roles the variable plays within the formula. These are of particular
#' importance, as they serve as special terms that can effect how a formula is
#' interpreted.
#'
#' | Role | Shortcut | Description |
#' | --- | --- | --- |
#' | outcome | `.o(...)` | exposure &rarr; __outcome__ |
#' | exposure | `.x(...)` | __exposure__ &rarr; outcome |
#' | predictor | `.p(...)` | exposure &plus; __predictor__ &rarr; outcome |
#' | confounder | `.c(...)` | exposure &larr; __confounder__ &rarr; outcome |
#' | mediator | `.m(...)` | exposure &rarr; __mediator__ &rarr; outcome |
#' | interaction | `.i(...)` | exposure &times; __interaction__ &rarr; outcome |
#' | strata | `.s(...)` | exposure &divide; __strata__ &rarr; outcome |
#' | _unknown_ |  | not yet assigned |
#'
#' Formulas can be condensed by applying their specific role to individual runes
#' as a function/wrapper. For example, `y ~ .x(x1) + x2 + x3`. This would
#' signify that `x1` has the specific role of an _exposure_.
#'
#' # Pluralized Arguments
#'
#' For a single argument, e.g. for the `tm.formula()` method, such as to
#' identify variable __X__ as an exposure, a `formula` should be given with the
#' term of interest on the LHS, and the description or instruction on the RHS.
#' This would look like `role = X ~ "exposure"`.
#'
#' For the arguments that would be dispatched for objects that are plural, e.g.
#' containing multiple terms, each `formula()` should be placed within a
#' `list()`. For example, the __role__ argument would be written:
#'
#' `role = list(X ~ "exposure", M ~ "mediator", C ~ "confounder")`
#'
#' Further implementation details can be seen in the implementation of
#' [list_of_formulas()].
#'
#' @param x An object that can be coerced to a `tm` object.
#'
#' @param role Specific roles the variable plays within the formula. These are
#'   of particular importance, as they serve as special terms that can effect
#'   how a formula is interpreted. Please see the _Roles_ section below for
#'   further details. The options for roles are as below:
#'
#'   * outcome
#'
#'   * exposure
#'
#'   * predictor
#'
#'   * confounder
#'
#'   * mediator
#'
#'   * interaction
#'
#'   * strata
#'
#'   * unknown
#'
#' @param side Which side of a formula should the term be on. Options are
#'   `c("left", "right", "meta", "unknown")`. The _meta_ option refers to a term
#'   that may apply globally to other terms.
#'
#' @param label Display-quality label describing the variable
#'
#' @param group Grouping variable name for modeling or placing terms together
#'
#' @param type Type of variable, either categorical (qualitative) or
#'   continuous (quantitative)
#'
#' @param distribution How the variable itself is more specifically
#'   subcategorized, e.g. ordinal, continuous, dichotomous, etc
#'
#' @param description Option for further descriptions or definitions needed for
#'   the tm, potentially part of a data dictionary
#'
#' @param transformation Modification of the term to be applied when
#'   combining with data
#'
#' @name tm
#' @export
tm <- function(x = unspecified(), ...) {
	UseMethod("tm", object = x)
}

#' @rdname tm
#' @export
tm.character <- function(x,
												 role = character(),
												 side = character(),
												 label = character(),
												 group = character(),
												 type = character(),
												 distribution = character(),
												 description = character(),
												 transformation = character(),
												 ...) {

	# Early Break if needed
	stopifnot("Missing/NA value not accepted for `tm` object" = !is.na(x))
	if (length(x) == 0) {
		return(new_tm())
	}

	# Redefine empty variables
	if (length(role) == 0) role <- "unknown"
	if (length(side) == 0) side <- "unknown"
	if (length(label) == 0) label <- NA
	if (length(group) == 0) group <- NA
	if (length(type) == 0) type <- NA
	if (length(distribution) == 0) distribution <- NA
	if (length(description) == 0) description <- NA
	if (length(transformation) == 0) transformation <- NA

	# Casting
	x <- vec_cast(x, character())
	role <- vec_cast(role, character())
	side <- vec_cast(side, character())
	label <- vec_cast(label, character())
	group <- vec_cast(group, character())
	description <- vec_cast(description, character())
	type <- vec_cast(type, character())
	distribution <- vec_cast(distribution, character())
	transformation <- vec_cast(transformation, character())

	new_tm(
		term = x,
		side = side,
		role = role,
		label = label,
		group = group,
		description = description,
		type = type,
		distribution = distribution,
		transformation = transformation,
	)
}

#' @rdname tm
#' @export
tm.formula <- function(x,
											 role = formula(),
											 label = formula(),
											 group = formula(),
											 type = formula(),
											 distribution = formula(),
											 description = formula(),
											 transformation = formula(),
											 ...) {

	# Early Break if needed
	if (length(x) == 0) {
		return(new_tm())
	}

	# Validate arguments and coerce into original assignments
	allArgs <- c(as.list(environment()), list(...))
	formalNames <-
		methods::formalArgs(tm.formula) |>
		utils::head(-1) |>
		utils::tail(-1)
	namedArgs <- allArgs[which(names(allArgs) %in% formalNames)]
	validate_classes(namedArgs, what = c("list", "formula"))

	# Turn all formula-based arguments into named lists
	role <- formulas_to_named_list(role)
	label <- formulas_to_named_list(label)
	group <- formulas_to_named_list(group)
	type <- formulas_to_named_list(type)
	distribution <- formulas_to_named_list(distribution)
	description <- formulas_to_named_list(description)
	transformation <- formulas_to_named_list(transformation)

	# Get actual formula components
	# Check to see if the RHS has any shortcut variables attached
	left <- lhs(x)
	right <- rhs(x)
	both <- c(left, right)

	# Roles/operations and need to be identified (on which terms they apply)
	# Output is named list (names = variable, list item = role|op)
	allRoles <-
		x |>
		all.names() |>
		{
			\(.x) {
				# These will be all roles
				var_names <- character()
				var_roles <- character()
				for (i in seq_along(.x)) {
					if (.x[i] %in% .roles) {
						var_names <- append(var_names, .x[i + 1])
						var_roles <- append(var_roles, .x[i])
					}
				}

				names(var_roles) <- var_names
				var_roles |>
					as.list()
			}
		}()

	# Supported transformations
	allOps <-
		x |>
		all.names() |>
		{
			\(.x) {
				# These will be all roles
				var_names <- character()
				var_roles <- character()
				for (i in seq_along(.x)) {
					if (.x[i] %in% .transformations) {
						var_names <- append(var_names, .x[i + 1])
						var_roles <- append(var_roles, .x[i])
					}
				}

				names(var_roles) <- var_names
				var_roles |>
					as.list()
			}
		}()

	# Combine to make supported variable names
	allNamed <- c(allRoles, allOps)

	# Remove role/op shortcut from terms (e.g. function in front of term)
	for (i in seq_along(allNamed)) {
		both[grepl(names(allNamed)[i], both)] <- names(allNamed)[i]
	}

	# Update left and rights
	for (i in both) {
		for (j in seq_along(left)) {
			if (grepl(i, left[j])) {
				left[j] <- i
			}
		}
		for (j in seq_along(right)) {
			if (grepl(i, right[j])) {
				right[j] <- i
			}
		}
	}

	# Find remaining terms that do not have a role
	# Right = Give them the role of a general predictor
	# Left = Make sure is an "outcome"
	for (j in left) {
		if (!(j %in% names(allRoles))) {
			allRoles[[j]] <- ".o"
		}
	}
	for (j in right) {
		if (!(j %in% names(allRoles))) {
			allRoles[[j]] <- ".p"
		}
	}

	# Warn and validate for interaction (as needs exposure variable)
	if (".i" %in% allNamed & !(".x" %in% allNamed)) {
		warning(
			"In interaction term was specified but was not attached to a specific exposure. The result will treat the interaction term as a regular predictor/covariate."
		)
	}

	# Re-name into full name
	for (i in seq_along(allRoles)) {
		allRoles[[i]] <- names(.roles[which(.roles %in% allRoles[[i]])])
	}

	# Setup to create new terms using all elements of original formula
	tm_vector <- new_tm()

	for (i in 1:length(both)) {
		# make parameters
		t <- both[i]

		# Sides
		sd <- if (t %in% names(allRoles[allRoles == "strata"])) {
			"meta"
		} else if (t %in% left) {
			"left"
		} else if (t %in% right) {
			"right"
		}

		# Data transforms
		op <- if (t %in% names(allOps)) {
			allOps[[t]]
		} else {
			NA
		}

		# Roles (every term has a role)
		rl <- allRoles[[t]]

		# groups
		grp <-
			if (t %in% names(group)) {
				groups[[t]]
			} else {
				NA
			}

		# Labels
		lb <- if (t %in% names(label)) {
			label[[t]]
		} else {
			NA
		}

		# place into rx list after casting appropriate classes
		tm_vector <- append(
			tm_vector,
			tm.character(
				x = vec_cast(t, character()),
				role = vec_cast(rl, character()),
				side = vec_cast(sd, character()),
				label = vec_cast(lb, character()),
				group = vec_cast(grp, character()),
				transformation = vec_cast(op, character()),
			)
		)

	}

	# return as a record of tm
	tm_vector
}

#' @rdname tm
#' @export
tm.default <- function(x = unspecified(), ...) {
	# Early break
	if (length(x) == 0) {
		return(new_tm())
	}

	stop("`tm()` is not defined for a `",
			 class(x)[1],
			 "` object.",
			 call. = FALSE
	)
}

#' record of formula tm
#' @keywords internal
#' @noRd
new_tm <- function(term = character(),
									 side = character(),
									 role = character(),
									 label = character(),
									 group = character(),
									 type = character(),
									 distribution = character(),
									 description = character(),
									 transformation = character(),
									 order = integer()) {

	# Validation
	vec_assert(term, ptype = character())
	vec_assert(role, ptype = character())
	vec_assert(side, ptype = character())
	vec_assert(label, ptype = character())
	vec_assert(description, ptype = character())
	vec_assert(type, ptype = character())
	vec_assert(distribution, ptype = character())
	vec_assert(transformation, ptype = character())
	vec_assert(order, ptype = integer())

	# Forced order
	if (length(term) > 0) {
		order <- 0L
	}

	new_rcrd(
		list(
			"term" = term,
			"role" = role,
			"side" = side,
			"label" = label,
			"description" = description,
			"type" = type,
			"distribution" = distribution,
			"transformation" = transformation,
			"order" = order
		),
		class = "tm"
	)
}

#' @rdname tm
#' @export
is_tm <- function(x) {
	inherits(x, "tm")
}

#' @export
format.tm <- function(x, ...) {
	tms <- vec_data(x)
	fmt <- character()

	if (vec_size(x) == 0) {
		fmt <- new_tm()
	} else if (has_cli() & vec_size(x) > 0) {
		for (i in 1:nrow(tms)) {
			if (tms$role[i] == "outcome") {
				t <- tms$term[i]
				fmt <- append(fmt, cli::col_yellow(t))
			}

			if (tms$role[i] == "exposure") {
				t <- tms$term[i]
				fmt <- append(fmt, cli::col_blue(t))
			}

			if (tms$role[i] == "predictor") {
				t <- tms$term[i]
				fmt <- append(fmt, cli::col_br_black(t))
			}

			if (tms$role[i] == "mediator") {
				t <- tms$term[i]
				fmt <- append(fmt, cli::col_cyan(t))
			}

			if (tms$role[i] == "confounder") {
				t <- tms$term[i]
				fmt <- append(fmt, cli::col_green(t))
			}

			if (tms$role[i] == "strata") {
				t <- tms$term[i]
				fmt <- append(fmt, cli::col_br_white(t))
			}

			if (tms$role[i] == "interaction") {
				t <- tms$term[i]
				fmt <- append(fmt, cli::col_br_blue(t))
			}

			if (tms$role[i] == "unknown") {
				t <- tms$term[i]
				fmt <- append(fmt, cli::col_black(t))
			}

		}
	} else {
		for (i in 1:nrow(tms)) {
			fmt <- append(fmt, tms$term[i])
		}
	}

	# return
	fmt
}

#' @export
obj_print_data.tm <- function(x, ...) {
	if (vec_size(x) == 0) {
		new_tm()
	} else if (vec_size(x) > 1) {
		cat(format(x), sep = "\n")
	} else {
		cat(format(x))
	}
}

#' @export
vec_ptype_full.tm <- function(x, ...) {
	"term"
}

#' @export
vec_ptype_abbr.tm <- function(x, ...) {
	"tm"
}


#' @export
vec_ptype2.tm.tm <- function(x, y, ...) x

#' @export
vec_cast.tm.tm <- function(x, to, ...) x

### character() ###

#' @export
vec_ptype2.tm.character <- function(x, y, ...) y # X = tm

#' @export
vec_ptype2.character.tm <- function(x, y, ...) x # X = character

#' @export
vec_cast.tm.character <- function(x, to, ...) {
	# order is flipped, such that `x` is character
	# Cast from character into terms
	attributes(x) <- NULL
	x[[1]]
}

#' @export
vec_cast.character.tm <- function(x, to, ...) {
	# order is flipped, such that `x` is tm
	attributes(x) <- NULL
	x[[1]]
}

### Term Helpers ---------------------------------------------------------------

#' @export
formula.tm <- function(x, ...) {

	# Lose information when converting to just character
	y <- vec_data(x)
	left <- paste0(y$term[y$side == "left"], collapse = " + ")
	right <- paste0(y$term[y$side == "right"], collapse = " + ")

	# Return formula
	stats::formula(paste0(left, " ~ ", right))

}

### Term List Wrapper Class ----------------------------------------------------

#' List of terms
#'
#' A simple `list_of` wrapper class around `tm` objects to allow them to be
#' vectorized.
#' @export
tmls <- function(...) {
	# Early break
	if (missing(..1)) {
		return(new_tmls(list()))
	}
	x <- vec_cast_common(..., .to = tm())

	new_tmls(x)
}

new_tmls <- function(...) {
	new_list_of(
		x = ...,
		ptype = tm(),
		class = "tmls"
	)
}

#' @export
vec_ptype_full.tmls <- function(x, ...) "term_list"

#' @export
vec_ptype_abbr.tmls <- function(x, ...) "tmls"

#' @keywords internal
methods::setOldClass(c("tmls", "vctrs_list_of"))

#' @export
format.tmls <- function(x, ...) {

	fmt <- character()
	# Character representation of formula
	if (vec_size(x) == 0) {
		return()
	} else if (vec_size(x) >= 1) {
		fmt <- lapply(x, format)
	}

	# Return
	fmt
}

#' @export
obj_print_data.tmls <- function(x, ...) {
	if (vec_size(x) == 0) {
		new_tmls()
	} else if (length(x) == 1) {
		y <- x[[1]]
		cat(format(y), sep = " | ")
	} else if (length(x) > 1) {
		lapply(x, FUN = function(.x) {
			cat(format(.x), sep = " | ")
			cat("\n")
		})
	}
}
