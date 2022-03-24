# Models -----------------------------------------------------------------------

#' Decorated Models
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' @name mx
#' @export
model_card <- function(x = unspecified(), ...) {
	UseMethod("model_card", object = x)
}

#' @rdname mx
#' @export
model_card.lm <- function(x,
													model_name = deparse(substitute(x)),
													term_labels = list(),
													term_roles = list(),
													model_label = character(),
													model_description = character(),
													...) {

	# Model should be wrapped in a list (thus of length = 1)
	m <- list(x)

	# Terms should be extracted and updated with the roles and labels as needed
	tm <- term(x, label = term_labels, role = term_roles)

	# Formula
	f <- stats::formula(x)

	# Type and subtypes
	type <- class(x)[1]
	subtype <- class(x)[2]

	# Create a shorter call object as a character
	cl <-
		type |>
		{\(.x) {
			as.call(list(str2lang(.x), f))
		}}() |>
		deparse1()

	# Make sure empty arguments are filled
	if (length(model_label) == 0)
		model_label <- NA_character_
	if (length(model_description) == 0)
		model_description <- NA_character_

	new_card(
		model = m,
		tag = model_name,
		terms = tm,
		type = type,
		subtype = subtype,
		label = model_label,
		description = model_description,
		call = cl,
		formulas = f
	)
}

#' @rdname mx
#' @export
model_card.glm = model_card.lm

#' @rdname mx
#' @export
model_card.model_fit <- function(x,
																 model_name = deparse(substitute(x)),
																 term_labels = list(),
																 term_roles = list(),
																 model_label = character(),
																 model_description = character(),
																 ...) {

	# Model should be wrapped in a list (thus of length = 1)
	m <- list(x)

	# Terms should be extracted and updated with the roles and labels as needed
	tm <- term(x, label = term_labels, role = term_roles)

	# Formula
	f <- stats::formula(x$fit)

	# Type and subtypes
	type <- class(x)[1]
	subtype <- class(x)[2]

	# Create a shorter call object that is a string
	cl <-
		c(subtype, type) |>
		{(\(.x) {
			stats::na.omit(.x) |>
			paste0(collapse = "")
		})}() |>
		{\(.x) {
			as.call(list(str2lang(.x), f))
		}}() |>
		deparse1()

	# Make sure empty arguments are filled
	if (length(model_label) == 0)
		model_label <- NA_character_
	if (length(model_description) == 0)
		model_description <- NA_character_

	new_card(
		model = m,
		tag = model_name,
		terms = tm,
		type = type,
		subtype = subtype,
		label = model_label,
		description = model_description,
		call = cl,
		formulas = f
	)
}
#' @rdname mx
#' @export
model_card.default <- function(x = unspecified(), ...) {

	# Early break if not viable method dispatch
	if (length(x) == 0) {
		return(new_card())
	} else {
		stop(
			"`model_card()` is not defined for a `", class(x)[1], "` object.",
			call. = FALSE
		)
	}
}


#' @rdname mx
#' @export
mx = model_card

# Vector Definition ------------------------------------------------------------

#' Model vector definition
#' @keywords internal
#' @noRd
new_card <- function(model = list(),
										 tag = character(),
										 type = character(),
										 subtype = character(),
										 label = character(),
										 description = character(),
										 call = character(),
										 terms = term(),
										 formulas = formula()) {

	# Internal to each model are specific data points
	vec_assert(model, ptype = list())
	vec_assert(tag, ptype = character())
	vec_assert(type, ptype = character())
	vec_assert(subtype, ptype = character())
	vec_assert(label, ptype = character())
	vec_assert(description, ptype = character())
	vec_assert(call, ptype = character())

	# Call and formula are not validated as vector attributes
	vec_assert(terms, ptype = term())
	validate_class(formulas, "formula")

	# Vector definition
	new_rcrd(
		list(
			"model" = model,
			"tag" = tag,
			"type" = type,
			"subtype" = subtype,
			"label" = label,
			"description" = description,
			"call" = call
		),
		terms = terms,
		formulas = formulas,
		class = "model_card"
	)

}

#' @keywords internal
#' @noRd
methods::setOldClass(c("model_card", "vctrs_vctr"))

# Casting and coercion ---------------------------------------------------------

#' @export
vec_ptype2.model_card.model_card <- function(x, y, ...) {
	x
}

#' @export
vec_cast.model_card.model_card <- function(x, to, ...) {
	x
}

# Output -----------------------------------------------------------------------

#' @export
format.model_card <- function(x, ...) {
	cl <- vec_data(x)$call
	cl

}

#' @export
obj_print_data.model_card <- function(x, ...) {

	if (vec_size(x) == 0) {
		new_card()
	}

	if (vec_size(x) >= 1) {
		cat(format(x), sep = "\n")
	} else {
		cat(format(x))
	}

}

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.model_card <- function(x, ...) {
	out <- format(x)
	pillar::new_pillar_shaft_simple(out, align = "left")
}

#' @export
vec_ptype_full.model_card <- function(x, ...) {
	"model_cards"
}

#' @export
vec_ptype_abbr.model_card <- function(x, ...) {
	"mx"
}
