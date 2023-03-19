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
#' | group | `.g(...)` | exposure &plus; __group__ &rarr; outcome |
#' | _unknown_ | `-` | not yet assigned |
#'
#' Formulas can be condensed by applying their specific role to individual runes
#' as a function/wrapper. For example, `y ~ .x(x1) + x2 + x3`. This would
#' signify that `x1` has the specific role of an _exposure_.
#'
#' Grouped variables are slightly different in that they are placed together in
#' a hierarchy or tier. To indicate the group and the tier, the shortcut can
#' have an `integer` following the `.g`. If no number is given, then it is
#' assumed they are all on the same tier. Ex: `y ~ x1 + .g1(x2) + .g1(x3)`
#'
#' __Warning__: Only a single shortcut can be applied to a variable within a
#' formula directly.
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
#'   * group
#'
#'   * unknown
#'
#' @param side Which side of a formula should the term be on. Options are
#'   `c("left", "right", "meta", "unknown")`. The _meta_ option refers to a term
#'   that may apply globally to other terms.
#'
#' @param label Display-quality label describing the variable
#'
#' @param group Grouping variable name for modeling or placing terms together.
#'   An integer value is given to identify which group the term will be in. The
#'   hierarchy will be `1` &rarr; `n` incrementally.
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
												 group = integer(),
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
	if (length(label) == 0) label <- NA_character_
	if (length(group) == 0) group <- NA_integer_
	if (length(type) == 0) type <- NA_character_
	if (length(distribution) == 0) distribution <- NA_character_
	if (length(description) == 0) description <- NA_character_
	if (length(transformation) == 0) transformation <- NA_character_

	# Casting
	x <- vec_cast(x, character())
	role <- vec_cast(role, character())
	side <- vec_cast(side, character())
	label <- vec_cast(label, character())
	group <- vec_cast(group, integer())
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

	# Roles/operations and need to be identified (on which terms they apply)
	# Output is named list (names = variable, list item = role|op)
	allRoles <-
		x |>
		all.names() |>
		{
			\(.x) {
				# These will be all roles
				varNames <- character()
				varRoles <- character()

				for (i in seq_along(.x)) {
					if (.x[i] %in% .roles) {
						varNames <- append(varNames, .x[i + 1])
						varRoles <- append(varRoles, .x[i])
					}
				}

				names(varRoles) <- varNames
				as.list(varRoles)
			}
		}()

	# Supported transformations
	allOps <-
		x |>
		all.names() |>
		{
			\(.x) {
				# These will be all roles
				varNames <- character()
				varRoles <- character()
				for (i in seq_along(.x)) {
					if (.x[i] %in% .transformations) {
						varNames <- append(varNames, .x[i + 1])
						varRoles <- append(varRoles, .x[i])
					}
				}

				names(varRoles) <- varNames
				as.list(varRoles)
			}
		}()

	# Grouped variables
	allGroups <-
		x |>
		all.names() |>
		{
			\(.x) {
				varNames <- character()
				varGroup <- character()

				# These will be all grouped variables
				grpInd <- grep("^\\.g$|^\\.g[[:digit:]]$", .x)
				varNames <- .x[grpInd + 1]
				varGroup <-
					.x[grpInd] |>
					{\(.y) gsub("\\.g$", ".g0", .y)}() |>
					substr(start = 3, stop = 3) |>
					as.integer()

				# Clean up group names to be alphanumeric and sequential
				names(varGroup) <- varNames
				as.list(varGroup)

			}
		}()

	# Handle interaction terms here (to match that of normal formulas)
	# 	Warn and validate for interaction (as needs exposure variable)
	# 	If both interaction and exposure available...
	# 		Create new interaction terms `a*b = a + b + a:b`
	# 		Create new grouping `.g(a) + .g(b) + .g(a:b)`
	#		Add back to original variables (allRoles, left, right, both)
	if (".i" %in% allRoles & !(".x" %in% allRoles)) {
		warning(
			"In interaction term was specified but was not attached to a specific exposure. The result will treat the interaction term as a regular predictor/covariate."
		)
	}

	if (".i" %in% allRoles & ".x" %in% allRoles) {

		exp <- names(allRoles[allRoles == ".x"])
		int <- names(allRoles[allRoles == ".i"])

		for (i in seq_along(exp)) {
			for (j in seq_along(int)) {

				# Explicit new interaction term
				.i <- paste0(exp[i], ":", int[j])
				allRoles[.i] <- ".i"

				# Correct interaction term changes in all related variables
				#		Remove prior interaction term that was now used
				#		Change to predictor
				#		Add interaction term to all terms = both + right
				allRoles[int[j]] <- ".p"
				right[grepl(names(allRoles[int[j]]), right)] <- int[j]
				right <- c(right, .i)

				# Add new group here (first find number, then addend)
				if (length(allGroups) == 0) {
					g <- 0
				} else {
					g <- max(unlist(unname(allGroups))) + 1
				}

				allGroups[exp[i]] <- g
				allGroups[int[j]] <- g
				allGroups[.i] <- g

			  message(
			    "Interaction term `",
			    int[j],
			    "` has been applied to exposure `",
			    exp[i],
			    "`"
			  )
			}
		}

	}

	# Remove roles and operations in term names
	for (i in seq_along(left)) {
		if (grepl("^\\.[[:lower:]]\\(", left[i])) {
			left[i] <- gsub("^\\.[[:lower:]]\\(", "", left[i])
			left[i] <- gsub("\\)$", "", left[i])
		}
	}
	for (i in seq_along(right)) {
		if (grepl("^\\.[[:lower:]]\\(", right[i])) {
			right[i] <- gsub("^\\.[[:lower:]]\\(", "", right[i])
			right[i] <- gsub("\\)$", "", right[i])
		}
	}

	# Find remaining terms that do not have a role
	# Right = Give them the role of a general predictor
	# Left = Make sure is an "outcome"
	for (i in left) {
		if (!(i %in% names(allRoles))) {
			allRoles[[i]] <- ".o"
		}
	}
	for (i in right) {
		if (!(i %in% names(allRoles))) {
			allRoles[[i]] <- ".p"
		}
	}

	# Combine all terms
	both <- c(left, right)


	# Re-name into full name
	for (i in seq_along(allRoles)) {
		allRoles[[i]] <- names(.roles[which(.roles %in% allRoles[[i]])])
	}

	# Correct for explicit roles specified in arguments
	for (i in seq_along(role)) {
		allRoles[[names(role[i])]] <- role[[i]]
	}


	# Setup to create new terms using all elements of original formula
	tm_vector <- tm()

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

		# Grouped terms
		grp <-
			if (t %in% names(group)) {
				group[[t]]
			} else if (t %in% names(allGroups)) {
				allGroups[[t]]
			}

		# Labels
		lb <- if (t %in% names(label)) {
			label[[t]]
		} else {
			NA
		}

		# Place into term list after casting appropriate classes
		tm_vector <- append(
			tm_vector,
			tm.character(
				x = vec_cast(t, character()),
				role = vec_cast(rl, character()),
				side = vec_cast(sd, character()),
				label = vec_cast(lb, character()),
				group = vec_cast(grp, integer()),
				transformation = vec_cast(op, character()),
			)
		)

	}

	# return as a record of tm
	tm_vector
}

#' @rdname tm
#' @export
tm.fmls <- function(x, ...) {
	field(x, "formula")[[1]]
}

#' @rdname tm
#' @export
tm.tm <- function(x, ...) {
	x
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
									 group = integer(),
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
	vec_assert(group, ptype = integer())
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
			"group" = group,
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
methods::setOldClass(c("tm", "vctrs_rcrd"))

### Coercion methods -----------------------------------------------------------

#' @export
vec_ptype2.tm.tm <- function(x, y, ...) x

#' @export
vec_cast.tm.tm <- function(x, to, ...) x

# CHARACTER

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

# FORMULA

#' @export
vec_ptype2.tm.formula <- function(x, y, ...) {
	x # A formula and term should vectorize to a term object
}

#' @export
vec_ptype2.formula.tm <- function(x, y, ...) {
	y # A formula and term should vectorize to a term object
}

#' @export
vec_cast.tm.formula <- function(x, to, ...) {
	# order is flipped, such that `x` is formula
	# Cast from formula into terms
	tm(x)
}

#' @export
vec_cast.formula.tm <- function(x, to, ...) {
	# order is flipped, such that `x` is tm
	formula(x)
}

# FMLS

#' @export
vec_ptype2.tm.fmls <- function(x, y, ...) {
	x # A fmls and term should vectorize to a term object
}

#' @export
vec_ptype2.fmls.tm <- function(x, y, ...) {
	y # A fmls and term should vectorize to a term object
}

#' @export
vec_cast.tm.fmls <- function(x, to, ...) {
	# order is flipped, such that `x` is fmls
	# Cast from fmls into terms
	tm(x)
}

#' @export
vec_cast.fmls.tm <- function(x, to, ...) {
	# order is flipped, such that `x` is tm
	# Cast from `tm` into `fmls`
	fmls(x)
}

### Term Helpers ---------------------------------------------------------------

#' @export
formula.tm <- function(x, ...) {

	# Lose information when converting to just character
  y <- vec_data(x)
  left <- paste0(y$term[y$side == "left"], collapse = " + ")
  right <-
    paste0(y$term[y$side == "right" |
                    y$side == "unknown"], collapse = " + ")

	# Return formula
	stats::formula(paste0(left, " ~ ", right))

}

#' Update `tm` objects
#'
#' This updates properties or attributes of a `tm` vector. This only updates
#' `tm` objects that already exist.
#'
#' @param ... A series of `field = term ~ value` pairs that represent the
#'   attribute to be updated. Can have a value of __<NA>__ if the goal is to
#'   remove an attribute or property.
#'
#' @return A `tm` object with updated attributes
#' @export
update.tm <- function(object, ...) {

	# Early break
	if (missing(..1) | length(..1) == 0) {
		return(object)
	}

	# Get update options and original data
	dots <- list(...)
	if (length(dots) == 1 && is.list(dots[[1]])) {
		dots <- dots[[1]]
	}
	termData <- vec_proxy(object)

	# Property management loop
	for (i in names(termData[-1])) {
		if (!is.null(dots[[i]])) {
			newProps <-
				dots[[i]] |>
				formulas_to_named_list()

			# Term management loop
			for (j in names(newProps)) {
				termData[termData$term == j, i] <- newProps[[j]]
			}
		}
	}

	# Restore and return
	vec_restore(termData, to = tm())
}

#' @importFrom generics components
#' @export
generics::components

#' Extract components from a vector of terms (`tm`)
#'
#' This is a [generics::components()] method for extract individual components from a formula.
#'
#' @param x A vector of `tm` objects
#'
#' @param ... A series of `name = value` pairs that represent the attribute to
#'   retrieve. Can have a value of __<NA>__ if needed to find missing attributes
#'   of terms. This is a filtering process, so all requirements will be returned
#'
#' @return  A `tm` object
#' @export
components.tm <- function(x, ...) {

	# Break early
	if (missing(..1)) {
		return(x)
	}

	dots <- list(...)
	termData <- vec_proxy(x)

	for (i in seq_along(dots)) {
		termData <- termData[termData[names(dots[i])] == dots[[i]], ]
	}

	# Restore and return
	vec_restore(termData, to = tm())

}

#' @importFrom generics intersect
#' @export
generics::intersect

#' @importFrom generics union
#' @export
generics::union

#' @importFrom generics setdiff
#' @export
generics::setdiff

#' Set operations for `tm` objects
#'
#' @param x,y Pair of `tm` vectors
#'
#' @param ... These dots are for future extensions and must be empty
#' @export
intersect.tm <- function(x, y, ...) {

# TODO
	c(x, y)

}


#' Describe attributes of a `tm` vector
#'
#' @param x A vector `tm` objects
#'
#' @param property A character vector of the following attributes of a `tm`
#'   object: role, side, label, group, description, type, distribution
#' @export
describe <- function(x, property) {

	validate_class(x, "tm")
	fieldNames <- fields(x)
	if (!(property %in% fieldNames)) {
		stop("`", property, "` is not an accessible property for this `tm` object.")
	}

	y <- vec_data(x)
	z <- y[[property]]
	names(z) <- y$term

	# Return in list format
	as.list(z)

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

#' @export
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
