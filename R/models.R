# Class ------------------------------------------------------------------------

#' Model prototypes
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' @name models
#' @export
mdl <- function(x = unspecified(),
								formulas = fmls(),
								data_name = character(),
								strata_info = list(),
								...) {

	if (length(x) == 0) {
		return(new_model())
	}

	# Validate classes
	validate_class(x, .models)
	validate_class(formulas, "fmls")

	# No current validation for
	#		if data is correct name
	# 	if strata is correct and have appropriate levels
	validate_class(data_name, "character")
	validate_class(strata_info, "list")
	if (length(strata_info) == 0) {
		strata_info <- list(strata = NA)
	}

	# Wrap model
	m <- list(x)

	# Creation
	new_model(
		model = m,
		model_type = class(x)[1],
		formulas = formulas,
		data_name = data_name,
		strata_info = strata_info
	)
}

#' @rdname models
#' @export
model <- mdl

# Vector Definition ------------------------------------------------------------

#' Model vector definition
#' @keywords internal
#' @noRd
new_model <- function(model = list(),
											formulas = fmls(),
											model_type = character(),
											data_name = character(),
											strata_info = list()) {

	# Validation
	vec_assert(model, ptype = list())
	vec_assert(model_type, ptype = character())
	vec_assert(formulas, ptype = fmls())
	vec_assert(data_name, ptype = character())
	vec_assert(strata_info, ptype = list())

	# Model archetype description is essentially deconstructed here
	# class = defined by the mdl, its base class, and a list
	# user defined descriptors = description
	# model defined descriptors = type, subtype
	# model level findings = statistics, formula
	# internals = terms, term descriptors... contained within the script
	new_rcrd(
		fields = list(
			"model" = model,
			"model_type" = model_type,
			"formulas" = formulas,
			"data_name" = data_name,
			"strata_info" = strata_info
		),
		class = "mdl"
	)
}

#' @keywords internal
#' @noRd
methods::setOldClass(c("mdl", "vctrs_vctr"))

# Output -----------------------------------------------------------------------

#' @export
format.mdl <- function(x, ...) {

	# Character representation of formula
	if (vec_size(x) == 0) {
		return()
	} else {
		fmt <-
			sapply(x, FUN = function(.x) {
				f <- field(.x, "formulas")
				cl <- field(.x, "model_type")
				paste0(cl, "(", f, ")")
			})
	}
	# Return
	fmt

}

#' @export
obj_print_data.mdl <- function(x, ...) {
	if (vec_size(x) == 0) {
		new_model()
	}

	if (vec_size(x) >= 1) {
		cat(format(x), sep = "\n")
	} else {
		cat(format(x))
	}
}

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.mdl <- function(x, ...) {
	out <- format(x)
	pillar::new_pillar_shaft_simple(out, align = "left")
}

#' @export
vec_ptype_full.mdl <- function(x, ...) {
	"model"
}

#' @export
vec_ptype_abbr.mdl <- function(x, ...) {
	"mdl"
}

# Casting and coercion ---------------------------------------------------------

#' @export
vec_ptype2.mdl.mdl <- function(x, y, ...) {
	x
}

#' @export
vec_cast.mdl.mdl <- function(x, to, ...) {
	x
}
