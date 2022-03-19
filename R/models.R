# Models -----------------------------------------------------------------------

#' Model records
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' @name mx
#' @export
model_rx <- function(x, ...) {
	UseMethod("model_rx", object = x)
}

#' @rdname mx
#' @export
model_rx.lm <- function(x,
												model_name = deparse(substitute(x)),
												term_labels = list(),
												term_roles = list(),
												model_label = character(),
												model_description = character(),
												...) {


	# Model should be wrapped in a list (thus of length = 1)
	m <- list(x)

	# Terms should be extracted and updated with the roles and labels as needed
	t <- term_rx(x)

	# Type and subtypes
	type <- class(x)[1]
	subtype <- class(x)[2]

	new_model(
		model = m,
		tag = model_name,
		terms = t,
		type = type,
		subtype = subtype,
		label = model_label,
		description = model_description
	)
}

#' @rdname mx
#' @export
model_rx.default <- function(x, ...) {

	# Early break if not viable method dispatch
	if (length(x) == 0) {
		return(new_model())
	} else {
		stop(
			"`model_rx()` is not defined for a `", class(x)[1], "` object.",
			call. = FALSE
		)
	}
}


#' @rdname mx
#' @export
mx = model_rx

# Vector Definition ------------------------------------------------------------

#' Model vector definition
#' @keywords internal
#' @noRd
new_model <- function(model = list(),
											tag = character(),
											terms = term_rx(),
											type = character(),
											subtype = character(),
											label = character(),
											description = character()) {


	# Validation
	vec_assert(model, ptype = list())
	vec_assert(tag, ptype = character())
	vec_assert(terms, ptype = term_rx())
	vec_assert(type, ptype = character())
	vec_assert(subtype, ptype = character())
	vec_assert(label, ptype = character())
	vec_assert(description, ptype = character())

	# Vector definition
	new_vctr(
		model,
		tag = tag,
		terms = terms,
		type = type,
		subtype = subtype,
		label = label,
		description = description
	)

}

#' @keywords internal
#' @noRd
methods::setOldClass(c("model_rx", "vctrs_vctr"))

# Output ----

#' @export
format.model_rx <- function(x, ...) {

	x |>
		vec_data() |>
		lapply(function(.x) {
			cl <- .x$call
			cl
		}) |>
		as.character()

}

#' @export
obj_print_data.model_rx <- function(x, ...) {

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
pillar_shaft.model_rx <- function(x, ...) {
	out <- format(x)
	pillar::new_pillar_shaft_simple(out, align = "left")
}

#' @export
vec_ptype_full.model_rx <- function(x, ...) {
	"model_rx"
}

#' @export
vec_ptype_abbr.model_rx <- function(x, ...) {
	"mx"
}
