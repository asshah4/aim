### Class Definition ----------------------------------------------------------

#' Create vectorized terms
#'
#' `r lifecycle::badge('experimental')`
#'
#' A vectorized term object that allows for additional information to be carried
#' with the variable name.
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
#' @name terms
#' @export
tm <- function(x = unspecified(), ...) {
	UseMethod("tm", object = x)
}

#' @rdname terms
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

#' @rdname terms
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
	# Uses zeallot for more compact code
	allArgs <- c(as.list(environment()), list(...))
	formalNames <-
		methods::formalArgs(tm.formula) |>
		utils::head(-1) |>
		utils::tail(-1)
	namedArgs <- allArgs[which(names(allArgs) %in% formalNames)]
	validate_classes(namedArgs, what = c("list", "formula"))
	modArgs <- lapply(namedArgs, function(.x) {
		.y <-
			list_of_formulas(.x) |>
			formula()

		sapply(.y, formula_to_named_list)
	})
	zeallot::`%<-%`(
		c(role, label, group, type, distribution, description, transformation),
		modArgs
	)

	# Get actual formula components
	# Check to see if the RHS has any shortcut variables attached
	left <- lhs(x)
	right <- rhs(x)

	# Roles/operations and need to be identified (on which terms they apply)
	# Output is named list (names = variable, list item = role|op)
	rightRoles <-
		x |>
		all.names() |>
		{
			\(.x) {
				# These will be named roles
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
	rightOps <-
		x |>
		all.names() |>
		{
			\(.x) {
				# These will be named roles
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

	# Combine to make supported right side
	rightSide <- c(rightRoles, rightOps)

	# Remove role/op shortcut from terms (e.g. function in front of term)
	for (i in seq_along(rightSide)) {
		right[grepl(names(rightSide)[i], right)] <- names(rightSide)[i]
	}

	# Find remaining right hand side variables that do not have a role
	# Give them the role of a general predictor
	for (i in seq_along(right)) {
		if (!(right[i] %in% names(rightRoles))) {
			rightRoles[right[i]] <- ".p"
		}
	}

	# Warn and validate for interaction (as needs exposure variable)
	if (".i" %in% rightSide & !(".x" %in% rightSide)) {
		warning(
			"In interaction term was specified but was not attached to a specific exposure. The result will treat the interaction term as a regular predictor/covariate."
		)
	}

	# Add roles for left hand side and combine into a list of all roles
	leftRoles <- rep(".o", length(left))
	names(leftRoles) <- left
	leftRoles <- as.list(leftRoles)
	roleList <- c(leftRoles, rightRoles) # All roles for all terms


	# Interaction term is already included by name
	for (i in seq_along(roleList)) {
		if (roleList[[i]] == ".o") {
			roleList[[i]] <- "outcome"
		}
		if (roleList[[i]] == ".x") {
			roleList[[i]] <- "exposure"
		}

		if (roleList[[i]] == ".m") {
			roleList[[i]] <- "mediator"
		}

		if (roleList[[i]] == ".c") {
			roleList[[i]] <- "confounder"
		}

		if (roleList[[i]] == ".p") {
			roleList[[i]] <- "predictor"
		}

		if (roleList[[i]] == ".s") {
			roleList[[i]] <- "strata"
		}

		if (roleList[[i]] == ".i") {
			roleList[[i]] <- "interaction"
		}
	}

	# Setup to create new terms using all elements of original formula
	both <- c(left, right)
	tm_vector <- new_tm()

	for (i in 1:length(both)) {
		# make parameters
		t <- both[i]

		# Sides
		sd <- if (t %in% names(roleList[roleList == "strata"])) {
			"meta"
		} else if (t %in% left) {
			"left"
		} else if (t %in% right) {
			"right"
		}

		# Data transforms
		op <- if (t %in% names(rightOps)) {
			rightOps[[t]]
		} else {
			NA
		}

		# Roles (every term has a role)
		rl <- roleList[[t]]

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

#' @rdname terms
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

#' @rdname terms
#' @export
is_tm <- function(x) {
	inherits(x, "tm")
}

#' @keywords internal
#' @noRd
methods::setOldClass(c("tm", "rcrds_rcrd"))

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
	"tm"
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

