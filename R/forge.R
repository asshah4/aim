# Model Forge ------------------------------------------------------------------

#' Forge Models into a Table
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' This function introduces a super class that combines both the `list` class
#' (and its derivative `list_of`) and regression models and/or hypothesis tests.
#' Models that are similar and share certain properties can be combined together
#' into a `model_forge`.
#'
#' @name model_forge
#' @export
model_forge <- function(x, ...) {
	UseMethod("model_forge", object = x)
}

#' @rdname model_forge
#' @export
model_forge.model_archetype <- function(x, ...) {

	mad <-
		vec_data(x) |>
		subset(select = c(model, tag, type, call, label, description, script)) |>
		tibble::tibble()

	mad$model[[1]]

	new_model_forge(
		x
	)

}

#' @rdname model_forge
#' @export
md_tbl = model_forge

# Vector List ------------------------------------------------------------------

#' Formula list
#' @keywords internal
#' @noRd
new_model_forge <- function(x) {

	stopifnot(is.data.frame(x))

	tibble::new_tibble(
		x,
		class = "model_forge",
		nrow = nrow(x)
	)



}

#' @keywords internal
#' @noRd
methods::setOldClass(c("model_forge", "vctrs_vctr"))
# Output -----------------------------------------------------------------------

#' @export
format.model_forge <- function(x, ...) {

	if (vec_size(x) == 0) {
	} else {
	}

	# Return
	x

}

#' @export
obj_print_data.model_forge <- function(x, ...) {
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
pillar_shaft.model_forge <- function(x, ...) {
	out <- format(x)
	pillar::new_pillar_shaft_simple(out, align = "left")
}

#' @export
vec_ptype_full.model_forge <- function(x, ...) {
	"model_forge"
}

#' @export
vec_ptype_abbr.model_forge <- function(x, ...) {
	"mx"
}


# Casting and coercion ---------------------------------------------------------

#' @export
vec_ptype2.model_forge.model_forge <- function(x, y, ...) {
	x
}

#' @export
vec_cast.model_forge.model_forge <- function(x, to, ...) {
	x
}



# Vctr-based model table ----

#' Map of many models
#'
#' @name map
#' @export
model_map <- function(x = list(), ...) {

	# Early break if needed
	if (length(x) == 0) {
		return(new_model_map())
	}

	# TODO
	# Potential input arguments are...
		# 1. Individual list of separate models, named or not
		# 2. Single <list_of_models> object, which may or may not be named
		# 3. Several <list_of_models> objects, named or not, in form of a list
		# 4. Mixed <list_of_models> and general models, named or not

	# Requires a list of models as initial workspace for casting into a table
	homogenous_list <-
		vapply(
			x,
			FUN = function(.x) {
				if (class(.x) %in% c("lm", "glm", "model_spec")) {
					TRUE
				} else {
					FALSE
				}
			},
			FUN.VALUE = TRUE
		)

	if (all(homogenous_list)) {
		if (inherits(x, "list_of_models")) {
			labs <- labels(x)
			rls <- roles(x)
			m <- cast.list_of_models(x)
		} else {
			x <- list_of_models(x)
			labs <- labels(x)
			rls <- roles(x)
			m <- cast.list_of_models(x)
		}
	}

	# From a basic table, change to a tidier table
	tidy_tbl <- m
	tidy_tbl$models <- tidy_models(m$models)
	tidy_tbl <-
		tidy_tbl |>
		tidyr::unnest(cols = models)

	# Return
	new_model_map(
		x = tidy_tbl,
		models = x,
		labels = labs,
		roles = rls
	)

}


#' Model map
#' @keywords internal
#' @noRd
new_model_map <- function(x = data.frame(),
													models = list(),
													labels = list(),
													roles = list()) {

	tibble::new_tibble(
		x,
		models = models,
		labels = labels,
		roles = roles,
		class = "model_map",
	)
}

#' @export
print.model_map <- function(x, ...) {

	cat(sprintf("<%s: %s models>\n", class(x)[[1]], length(attr(x, "models"))))
	cli::cat_line(format(x)[-1])
}


