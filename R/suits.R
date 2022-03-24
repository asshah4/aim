# Model Lists ------------------------------------------------------------------

#' Model Lists
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' This function introduces a super class that combines both the `list` class
#' (and its derivative `list_of`) and regression models and/or hypothesis tests.
#' Models that are similar and share certain properties can be combined together
#' into a `model_suit`.
#'
#' @name model_suit
#' @export
model_suit <- function(x, ...) {
	UseMethod("model_suit", object = x)
}

#' @rdname model_suit
#' @export
model_suit.formula_list <- function(x,
																		name = deparse1(substitute(x)),
																		fitting_function,
																		...,
																		data) {
	# Fit the models first
	nms <- names(x)
	out <- fit(x, fitting_function = fitting_function, data = data)

	# Create a list of fitted model cards
	ml <- lapply(out, FUN = function(.x) {
		model_card(.x)
	})

	# Obtain the terms from the models or from the original formula_list
	tm <- term(x)

	# Recreate a formula summary
	f <-
		paste(paste0(lhs(tm), collapse = " + "),
					paste0(rhs(tm), collapse = " + "),
					sep = " ~ ") |>
		stats::as.formula()

	# Return
	new_suit(
		model_suit = ml,
		terms = tm,
		formulas = f
	)
}

#' @rdname model_suit
#' @export
model_suit.default <- function(x, ...) {

	# Early break if not viable method dispatch
	if (length(x) == 0) {
		return(new_suit())
	} else {
		stop(
			"`model_suit()` is not defined for a `", class(x)[1], "` object.",
			call. = FALSE
		)
	}
}

#' @rdname model_suit
#' @export
mdls = model_suit

# Vector List ------------------------------------------------------------------

#' Formula list
#' @keywords internal
#' @noRd
new_suit <- function(model_suit = list(),
										 terms = term(),
										 formulas = formula()) {

	# Each model suit needs to have similar members for further evaluation
	vec_assert(terms, ptype = term())
	validate_class(formulas, "formula")

	new_list_of(
		x = model_suit,
		ptype = model_card(),
		terms = terms,
		formulas = formulas,
		class = "model_suit"
	)

}

#' @keywords internal
#' @noRd
methods::setOldClass(c("model_suit", "vctrs_vctr"))

# Output -----------------------------------------------------------------------

#' @export
format.model_suit <- function(x, ...) {

	if (vec_size(x) == 0) {
		fmt_ms <- new_suit()
	} else {
		fmt_ms <-
			sapply(vec_data(x), function(.x) {
				cl <- vec_data(.x)$call

				if (has_cli()) {
					cli::col_br_white(cl)
				} else {
					cl
				}
			})
	}

	# Return
	fmt_ms

}

#' @export
obj_print_data.model_suit <- function(x, ...) {
	if (length(x) == 0) {
		return()
	}

	if (length(x) >= 1) {
		cat(format(x), sep = "\n")
	} else {
		cat(format(x))
	}
}

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.model_suit <- function(x, ...) {
	out <- format(x)
	pillar::new_pillar_shaft_simple(out, align = "left")
}

#' @export
vec_ptype_full.model_suit <- function(x, ...) {
	"model_suit"
}

#' @export
vec_ptype_abbr.model_suit <- function(x, ...) {
	"mdls"
}

