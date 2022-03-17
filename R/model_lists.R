#' Model Lists
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' This function introduces a super  that combines both the `list` class (and
#' its derivative `list_of`) and regression models
#'
#' @name model_list
#' @export
model_list <- function(x, ...) {
	UseMethod("model_list", object = x)
}

#' @rdname model_list
#' @export
model_list.list <- function(x,
														labels = list(),
														roles = list(),
														name = deparse1(substitute(x)),
														...) {


	# Attributes
	labs <- labels(x)
	rls <- forks::roles(x)
	grps <- forks::groups(x)

	new_model_list(
		model_list = ml,
		labels = labs,
		roles = rls
	)

}

#' @rdname model_list
#' @export
model_list.default <- function(x, ...) {

	# Early break if not viable method dispatch
	if (length(x) == 0) {
		return(new_model_list())
	} else {
		stop(
			"`model_list()` is not defined for a `", class(x)[1], "` object.",
			call. = FALSE
		)
	}
}

#' @rdname model_list
#' @export
mdls = model_list

# vctrs ----

#' Formula list
#' @keywords internal
#' @noRd
new_model_list <- function(model_list = list(),
													 labels = list(),
													 roles = list()) {

	new_list_of(
		x = model_list,
		ptype = list(),
		class = "model_list",
		names = names,
		labels = labels,
		roles = roles
	)

}

#' @keywords internal
#' @noRd
methods::setOldClass(c("model_list", "vctrs_vctr"))

# Output ----

#' @export
format.model_list <- function(x, ...) {

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
obj_print_data.model_list <- function(x, ...) {
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
pillar_shaft.model_list <- function(x, ...) {
	out <- format(x)
	pillar::new_pillar_shaft_simple(out, align = "left")
}

#' @export
vec_ptype_full.model_list <- function(x, ...) {
	"model_list"
}

#' @export
vec_ptype_abbr.model_list <- function(x, ...) {
	"mdls"
}

