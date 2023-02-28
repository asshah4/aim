### Class definition ----------------------------------------------------------

#' List of formulas
#'
#' Vectorized subclass of the `list_of` class from [vctrs::list_of()] that
#' utilizes a formula pattern for containing information or relationships.
#'
#' For arguments that may need to be pluralized, the arguments should be input
#' as a formula, or as a list of formulas. The purpose of this format is to give
#' multiple descriptions or instructions at once.
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

	if (class(..1) == "list") {
		x <-
			Filter(length, ...) |>
			sapply(vec_cast_common, .to = fmls())

	} else {
		x <- vec_cast_common(..., .to = fmls())
	}

	# Return
	new_lst_fmls(x)

}

#' @rdname list_of_formulas
list_of_formulas = lst_fmls

#' @keywords internal
#' @noRd
new_lst_fmls <- function(...) {

	new_list_of(
		x = ...,
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
		cat(format(y), sep = " | ")
	} else if (length(x) > 1) {
		lapply(x, FUN = function(.x) {
			cat(format(.x), sep = " | ")
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
