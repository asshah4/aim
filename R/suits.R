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

	ml <- model_card()
	for (i in 1:length(out)) {
		m <- model_card(out[[i]])
		ml <- append(ml, m)
	}

	# Obtain the terms from the models as well
	tl <- term()
	for (i in 1:length(out)) {
		t <- term(out[[i]])
		tl <- append(tl, t)
	}
	tm <- unique(tl)
	f <- paste(paste0(lhs(tm), collapse = " + "),
						 paste0(rhs(tm), collapse = " + "),
						 sep = " ~ ")


	new_suit(
		model_suit = ml,
		terms = tm,
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

# vctrs ----

#' Formula list
#' @keywords internal
#' @noRd
new_suit <- function(model_suit = model_card(),
										 model_type = character(),
										 terms = term()) {

	# Each model suit needs to have similar members for further evaluation
	# Each group would need to have ...
		# Type = modeling class (e.g. lm, glm, etc)
		# Key variable = either same outcome or same major exposure
		# Labels = common term labels

	new_list_of(
		x = model_suit,
		ptype = model_card(),
		terms = terms,
		class = "model_suit"
	)

}

#' @keywords internal
#' @noRd
methods::setOldClass(c("model_suit", "vctrs_vctr"))

# Output ----

#' @export
format.model_suit <- function(x, ...) {

	ms <- vec_data(x)
	fmt_ms <- character()

	if (vec_size(x) == 0) {
		fmt_ms <- new_suit()
	} else if (has_cli() & vec_size(x) > 0) {
		for (i in 1:nrow(ms)) {
			fmt_ms <- append(fmt_ms, cli::col_br_white(ms$call[i]))
		}
	} else {
		for (i in 1:nrow(ms)) {
			fmt_ms <- append(fmt_ms, ms$call[i])
		}
	}

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

