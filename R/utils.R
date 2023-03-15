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

	# Handle special interaction terms in original formula
	# Will expand from "a * b" -> "a + b + a:b"
	pos <- grep("\\*", y)
	npos <- grep("\\*", y, invert = TRUE)

	ints <- character()
	if (length(pos) > 0) {
	  ints <-
	    y[pos] |>
	    strsplit("\\*") |>
	    unlist() |>
	    trimws() |>
	    {
	      \(.x) c(.x[1], .x[2], paste0(.x[1], ":", .x[2]))
	    }()
	}

	# Return
	c(ints, y[npos])

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
			if (grepl("^[[:digit:]]$", val)) {
				val <- as.integer(val)
			}
			names(val) <- nm
			val <- as.list(val)
		})
	}

	# Return
	y
}
