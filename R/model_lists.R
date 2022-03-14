#' Model Lists
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' This function introduces a super  that combines both the `list` class (and
#' its derivative `list_of`) and regression models
#'
#' @name list_of_models
#' @export
list_of_models <- function(x, ...) {
	UseMethod("list_of_models", object = x)
}

#' @rdname list_of_models
#' @export
list_of_models.list <- function(x,
								labels = list(),
								roles = list(),
								name = deparse1(substitute(x)),
								...) {

	# Attributes
	labs <- labels(x)
	rls <- forks::roles(x)
	grps <- forks::groups(x)

	new_list_of_models(
		model_list = ml,
		labels = labs,
		roles = rls
	)

}

#' @rdname list_of_models
#' @export
list_of_models.default <- function(x, ...) {

	# Early break if not viable method dispatch
	if (length(x) == 0) {
		return(new_list_of_models())
	} else {
		stop(
			"`list_of_models()` is not defined for a `", class(x)[1], "` object.",
			call. = FALSE
		)
	}
}

#' @rdname list_of_models
#' @export
mdls = list_of_models

# vctrs ----

#' Formula list
#' @keywords internal
#' @noRd
new_list_of_models <- function(model_list = list(),
							   labels = list(),
							   roles = list()) {

	new_list_of(
		x = model_list,
		ptype = list(),
		class = "list_of_models",
		labels = labels,
		roles = roles
	)

}

#' @keywords internal
#' @noRd
methods::setOldClass(c("list_of_models", "vctrs_vctr"))

# Output ----

#' @export
format.list_of_models <- function(x, ...) {

	ml <-
		x |>
		vec_data() |>
		lapply(function(.x) {
			cl <- .x[["call"]]
			cl
		}) |>
		unname() |>
		as.character()

	ml

}

#' @export
obj_print_data.list_of_models <- function(x, ...) {
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
pillar_shaft.list_of_models <- function(x, ...) {
	out <- format(x)
	pillar::new_pillar_shaft_simple(out, align = "left")
}

#' @export
vec_ptype_full.list_of_models <- function(x, ...) {
	"list_of_models"
}

#' @export
vec_ptype_abbr.list_of_models <- function(x, ...) {
	"mdls"
}

