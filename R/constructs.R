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
#' into a `model_construct`.
#'
#' @name model_construct
#' @export
model_construct <- function(x, ...) {
	UseMethod("model_construct", object = x)
}

#' @rdname model_construct
#' @export
model_construct.formula_list <- function(x,
																				 name = deparse1(substitute(x)),
																				 fitting_function,
																				 ...,
																				 data) {

	# Fit the models first
	nms <- names(x)

	# Create a list of fitted model cards
	out <- fit(x, fitting_function = fitting_function, data = data)
	ml <- lapply(out, FUN = function(.x) {
		model_archetype(.x)
	})

	# Check compatability
	# Will stop/error if too dissimilar
	validate_model_compatability(ml)

	# Obtain the terms from the models or from the original formula_list
	tm <- term(x)

	# Recreate a formula summary
	f <-
		paste(paste0(lhs(tm), collapse = " + "),
					paste0(rhs(tm), collapse = " + "),
					sep = " ~ ") |>
		stats::as.formula()

	# Return
	new_model_construct(
		model_construct = ml,
		terms = tm,
		formulas = f
	)
}

#' @rdname model_construct
#' @export
model_construct.default <- function(x, ...) {

	# Early break if not viable method dispatch
	if (length(x) == 0) {
		return(new_model_construct())
	} else {
		stop(
			"`model_construct()` is not defined for a `", class(x)[1], "` object.",
			call. = FALSE
		)
	}
}

#' @rdname model_construct
#' @export
mdls = model_construct

# Vector List ------------------------------------------------------------------

#' Formula list
#' @keywords internal
#' @noRd
new_model_construct <- function(model_construct = list(),
																terms = term(),
																formulas = formula()) {

	# Each model suit needs to have similar members for further evaluation
	vec_assert(terms, ptype = term())
	validate_class(formulas, "formula")

	new_list_of(
		x = model_construct,
		ptype = model_archetype(),
		terms = terms,
		formulas = formulas,
		class = "model_construct"
	)

}

#' @keywords internal
#' @noRd
methods::setOldClass(c("model_construct", "vctrs_vctr"))

# Output -----------------------------------------------------------------------

#' @export
format.model_construct <- function(x, ...) {

	if (vec_size(x) == 0) {
		fmt_mc <- new_model_construct()
	} else {
		fmt_mc <-
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
	fmt_mc

}

#' @export
obj_print_data.model_construct <- function(x, ...) {
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
pillar_shaft.model_construct <- function(x, ...) {
	out <- format(x)
	pillar::new_pillar_shaft_simple(out, align = "left")
}

#' @export
vec_ptype_full.model_construct <- function(x, ...) {
	"model_construct"
}

#' @export
vec_ptype_abbr.model_construct <- function(x, ...) {
	"mdls"
}


# Casting and coercion ---------------------------------------------------------

#' @export
vec_ptype2.model_construct.model_construct <- function(x, y, ...) {
	x
}

#' @export
vec_cast.model_construct.model_construct <- function(x, to, ...) {
	x
}
