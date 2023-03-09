### Class definition ----------------------------------------------------------

#' List of formulas
#'
#' Vectorized subclass of the `list_of` class from [vctrs::list_of()] that
#' utilizes a formula pattern for containing information or relationships. This
#' serves as a base class that can be expanded as needed.
#'
#' @name list_of_formulas
#' @export
lst_fmls <- function(...) {

	# The input here can be quite varied. Our goal is to flatten an unnested list
	# Options are:
	# 	Empty
	# 	Empty list
	# 	Single formula
	#		List of formulas
	# 	`fmls` (single or multiple)
	# 	Incorrect type of object

	# Early break for empty
	if (missing(..1)) {
		return(new_lst_fmls(list()))
	} else if (length(..1) == 0) {
		return(new_lst_fmls(list()))
	}

	dots <- eval(substitute(alist(...)))

	inheritanceTypes <- sapply(
		dots,
		FUN = function(.x) {
			.y <- eval(.x)
			class(.y)[1]
		},
		USE.NAMES = FALSE
	)

	familyTypes <- sapply(
		dots,
		FUN = function(.x) {
			.y <- eval(.x)

			if (class(.y)[1] == "fmls") {
				.z <- unique(field(.y, "pattern"))
			} else if (class(.y)[1] == "formula") {
				.z <- "user_defined"
			}

			.z
		},
		USE.NAMES = FALSE
	)

	complexityLevels <- sapply(
		dots,
		FUN = function(.x) {
			.y <- eval(.x)
			complexity(.y)
		},
		USE.NAMES = FALSE
	)
	# complexityLevel <- max(sapply(..., FUN = complexity, USE.NAMES = FALSE))

	# # Get inheritance type, either `formula` or `fmls`
	# inheritanceType <- class(..1)[1]
	#
	# if (inheritanceType == "fmls") {
	#
	# 	# Pattern to family
	# 	patternVector <- field(..., "pattern")
	#
	# } else if (inheritanceType == "fmls" & ...length() > 1) {
	#
	# # Identify the pattern used if a `fmls`
	# if (inheritanceType == "fmls" & ...length() == 1) {
	#
	# 	# Pattern to family
	# 	familyType <- field(..., "pattern")
	#
	# } else if (inheritanceType == "fmls" & ...length() > 1) {
	#
	# 	# Pattern identification
	# 	patternVector <- unique(sapply(
	# 		list(...),
	# 		FUN = function(.x) {
	# 			unique(field(.x, "pattern"))
	# 		},
	# 		USE.NAMES = FALSE
	# 	))
	#
	#
	# 	# For multiple, non-expanded patterns
	# 	if (length(patternVector) > 1) {
	# 		familyType <- "mixed"
	# 	} else {
	# 		familyType <- patternVector
	# 	}
	#
	# } else { # For cases defined by user, when not using `fmls` as base class
	# 	familyType <- "user_defined"
	# }
	#
	# # Complexity of any variable type
	# # Max complexity to be returned
	# complexityLevel <- max(sapply(..., FUN = complexity, USE.NAMES = FALSE))

	formulaList <- sapply(
		dots,
		FUN = function(.x) {
			.y <- eval(.x)
			vec_cast_common(.y, .to = fmls())
		},
		USE.NAMES = FALSE
	)


	# Return
	new_lst_fmls(x = formulaList,
							 inheritance = inheritanceTypes,
							 family = familyTypes,
							 complexity = complexityLevels)

}

#' @rdname list_of_formulas
list_of_formulas = lst_fmls

#' @keywords internal
#' @noRd
new_lst_fmls <- function(x,
												 inheritance = NA_character_,
												 family = NA_character_,
												 complexity = NA_integer_) {

	new_list_of(
		x = x,
		inheritance = inheritance,
		family = family,
		complexity = complexity,
		ptype = fmls(),
		class = "lst_fmls"
	)

}


#' @keywords internal
#' @noRd
methods::setOldClass(c("lst_fmls", "vctrs_list_of"))

#' @export
format.lst_fmls <- function(x, ...) {

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
obj_print_data.lst_fmls <- function(x, ...) {

	if (vec_size(x) == 0) {
		new_lst_fmls()
	} else if (length(x) == 1) {
		y <- x[[1]]
		cat(format(y), sep = " || ")
	} else if (length(x) > 1) {
		lapply(x, FUN = function(.x) {
			cat(format(.x), sep = " || ")
			cat("\n")
		})
	}
}

#' @export
vec_ptype_full.lst_fmls <- function(x, ...) {
	"list_of_formulas"
}

#' @export
vec_ptype_abbr.lst_fmls <- function(x, ...) {
	"lst_fmls"
}

### Coercion methods -----------------------------------------------------------

# SELF

#' @export
vec_ptype2.lst_fmls.list_of_forulas <- function(x, y, ...) {
	new_lst_fmls()
}

#' @export
vec_cast.lst_fmls.lst_fmls <- function(x, to, ...) {
	x
}

# FORMULA

#' @export
vec_ptype2.lst_fmls.formula <- function(x, y, ...) {
	# `lst_fmls` + `formula` should lead to a `formula`
	y
}

#' @export
vec_ptype2.formula.lst_fmls <- function(x, y, ...) {
	# `formula` + `lst_fmls` should lead to a `formula`
	x
}

#' @export
vec_cast.formula.lst_fmls <- function(x, to, ...) {
	# Cast from `lst_fmls` into `formula`
	lapply(x, function(.x) {
		.x |>
			tm() |>
			stats::formula()
	})
}

#' @export
vec_cast.lst_fmls.formula <- function(x, to, ...) {
	# Cast from `formula` into `lst_fmls`
	lst_fmls(x)
}

### List of formula helper functions -------------------------------------------

#' @export
formula.lst_fmls <- function(x, ...) {
	lapply(x, function(.x) {
		.x |>
			tm() |>
			stats::formula()
	})
}
