# Models -----------------------------------------------------------------------

#' Model archetype
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' @name md
#' @export
model_archetype <- function(x = unspecified(), ...) {
	UseMethod("model_archetype", object = x)
}

#' @rdname md
#' @export
model_archetype.lm <- function(x,
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

	new_model_archetype(
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

#' @rdname md
#' @export
model_archetype.glm <- model_archetype.lm

#' @rdname md
#' @export
model_archetype.model_fit <- function(x,
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

	new_model_archetype(
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
#' @rdname md
#' @export
model_archetype.default <- function(x = unspecified(), ...) {

	# Early break if not viable method dispatch
	if (length(x) == 0) {
		return(new_model_archetype())
	} else {
		stop(
			"`model_archetype()` is not defined for a `", class(x)[1], "` object.",
			call. = FALSE
		)
	}
}


#' @rdname md
#' @export
md = model_archetype

# Vector Definition ------------------------------------------------------------

#' Model vector definition
#' @keywords internal
#' @noRd
new_model_archetype <- function(model = list(),
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
		class = "model_archetype"
	)

}

#' @keywords internal
#' @noRd
methods::setOldClass(c("model_archetype", "vctrs_vctr"))

# Output -----------------------------------------------------------------------

#' @export
format.model_archetype <- function(x, ...) {
	cl <- vec_data(x)$call
	cl

}

#' @export
obj_print_data.model_archetype <- function(x, ...) {

	if (vec_size(x) == 0) {
		new_model_archetype()
	}

	if (vec_size(x) >= 1) {
		cat(format(x), sep = "\n")
	} else {
		cat(format(x))
	}

}

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.model_archetype <- function(x, ...) {
	out <- format(x)
	pillar::new_pillar_shaft_simple(out, align = "left")
}

#' @export
vec_ptype_full.model_archetype <- function(x, ...) {
	"model_archetype"
}

#' @export
vec_ptype_abbr.model_archetype <- function(x, ...) {
	"md"
}

# Casting and coercion ---------------------------------------------------------

#' @export
vec_ptype2.model_archetype.model_archetype <- function(x, y, ...) {
	x
}

#' @export
vec_cast.model_archetype.model_archetype <- function(x, to, ...) {
	x
}
