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

	# Early break if needed
	dots <- list(...)
	if (length(dots) == 0) {
		return(new_lst_fmls())
	} else if (inherits(dots[[1]], "lst_fmls")) {
		return(dots[[1]])
	}
	if (length(dots) == 1 && is.list(dots[[1]])) {
		dots <- dots[[1]]
	}

	# Validate contents
	stopifnot("Should be applied to individual or list of formulas" =
							inherits(f, c("list", "formula")))

	listOfFormulas <- lapply(
		dots,
		FUN = function(.x) {

			# Get components
			.l <- lhs(.x)
			.r <- rhs(.x)

			# If there are spaces within an term, it has to have escapable quotes
			.y <- deparse1(.x)

		}
	)

	new_lst_fmls(listOfFormulas)

}

#' @rdname list_of_formulas
list_of_formulas = lst_fmls

#' @keywords internal
#' @noRd
new_lst_fmls <- function(...) {

	new_list_of(
		x = list(...),
		ptype = character(),
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
	} else {
		fmt <- sapply(x, FUN = as.character)
	}

	# Return
	fmt
}

#' @export
obj_print_data.lst_fmls <- function(x, ...) {
	if (vec_size(x) == 0) {
		new_lst_fmls()
	}

	if (vec_size(x) >= 1) {
		cat(format(x), sep = "\n")
	} else {
		cat(format(x))
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

#' @export
vec_ptype2.lst_fmls.list_of_forulas <- function(x, y, ...) {
	new_lst_fmls()
}

#' @export
vec_cast.lst_fmls.lst_fmls <- function(x, to, ...) {
	x
}

### List Helpers --------------------------------------------------------------

#' @export
formula.lst_fmls <- function(x, ...) {

	y <- list()
	for (i in seq_along(x)) {
		y <- append(y, x[[i]])
	}

	# Each element of y will be a character
	lapply(y, FUN = function(.x) {
		as.formula(.x)
		# .l <-
		# 	.x |>
		# 	strsplit("~") |>
		# 	unlist() |>
		# 	head(1) |>
		# 	trimws() |>
		# 	{
		# 		\(.y) gsub('"', "", .y)
		# 	}()
		#
		# .r <-
		# 	.x |>
		# 	strsplit("~") |>
		# 	unlist() |>
		# 	tail(-1) |>
		# 	trimws()
		#
		# for (i in seq_along(.r)) {
		# 	if (grepl(' ', .r[i])) {
		# 		.r[i] <- shQuote(.r)
		# 	}
		# }
		#
		# stats::reformulate(.r, .l)
	})

}

#' Take list of formula, and return as a named list (name = LHS, value = RHS)
#' @export
formulas_to_named_list <- function(x) {

	# Check to see if its a single formula or a list of formulas
	stopifnot("Should be applied to individual or list of formulas" =
							inherits(x, c("list", "formula")))

	# Empty, list, or formula management
	if (length(x) == 0) { # If an empty formula or list, return an empty list
		y <- list()
	} else if (inherits(x, "formula")) { # If a single formula
		nm <- lhs(x)
		val <- rhs(x)
		names(val) <- nm
		y <- as.list(val)
	} else if (inherits(x, "lst_fmls")) { # if it is a `lst_fmls` object
		y <-
			stats::formula(x) |>
			sapply(function(.x) {
				nm <- lhs(.x)
				val <- rhs(.x)
				names(val) <- nm
				val <- as.list(val)
			})
	} else if (inherits(x, "list")) { # if a list that contains formulas
		y <- sapply(x, function(.x) {
			nm <- lhs(.x)
			val <- rhs(.x)
			names(val) <- nm
			val <- as.list(val)
		})
	}

	# Return
	y
}

### Formula Helpers -----------------------------------------------------------

#' Tools for working with formula-like objects
#' @name formula_helpers
#' @export
lhs <- function(x, ...) {
	UseMethod("lhs", object = x)
}

#' @rdname formula_helpers
#' @export
rhs <- function(x, ...) {
	UseMethod("rhs", object = x)
}

#' @rdname formula_helpers
#' @export
rhs.formula <- function(x, ...) {

	# Handles name, call, and character options
	# Does strip away parentheses
	y <-
		x[[length(x)]] |>
		deparse1() |>
		strsplit("\\+|-") |>
		unlist() |>
		trimws() |>
		{
			\(.x) gsub('"', "", .x)
		}()

	y
}

#' @rdname formula_helpers
#' @export
lhs.formula <- function(x, ...) {
	if (length(x) == 2) {
		y <- character()
	} else if (length(x) == 3) {
		y <- x[[2]]
	}

	z <-
		y |>
		deparse1() |>
		strsplit("\\+|-") |>
		unlist() |>
		trimws() |>
		{
			\(.x) gsub('"', "", .x)
		}()

	z
}
