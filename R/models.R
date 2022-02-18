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
																...) {

}

#' @rdname list_of_models
#' @export
list_of_models.list_of_formulas <- function(x,
																						.f,
																						...,
																						data) {

	# Validation will happen inside fitting function
	ml <- fit(x, .f = .f, ..., data = data)

	# Attributes
	labs <- labels(x)
	rls <- forks::roles(x)
	grps <- forks::groups(x)

	new_list_of_models(
		model_list = ml,
		labels = labs,
		roles = rls,
		groups = grps
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
															 roles = list(),
															 groups = list()) {

	new_list_of(
		x = model_list,
		ptype = list(),
		class = "list_of_models",
		labels = labels,
		roles = roles,
		groups = groups
	)

}

#' @keywords internal
#' @noRd
methods::setOldClass(c("list_of_models", "vctrs_vctr"))

# Output ----

#' @export
format.list_of_models <- function(x, ...) {

	f <- lapply(vec_data(x), function(.x) {
		attributes(.x) <- NULL
		.x
	})
	f <- unname(f)
	f <- as.character(f)
	f

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

#' @export
vec_ptype_full.list_of_models <- function(x, ...) {
	"list_of_models"
}

#' @export
vec_ptype_abbr.list_of_models <- function(x, ...) {
	"mdls"
}


#' @export
format.list_of_models <- function(x, ...) {

	lapply(x, FUN = function(.x) {
		print(.x)
	})

}

#' @export
vec_ptype_full.list_of_models <- function(x, ...) {
	"list_of_models"
}

#' @export
vec_ptype_abbr.list_of_models <- function(x, ...) {
	"mdls"
}

# Utility Functions ----

#' @export
labels.list_of_models <- function(x, ...) {
	attr(x, "labels")
}

#' @export
roles.list_of_models <- function(x, ...) {
	attr(x, "roles")
}

#' @export
groups.list_of_models <- function(x, ...) {
	attr(x, "groups")
}
