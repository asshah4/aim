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


