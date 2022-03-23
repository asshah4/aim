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


#' @export
rebuild.list_of_models <- function(x, ...) {

	# Basic extraction
	nm <- names(x)

	# Roles
	rls <- roles(x)
	out <- names(rls[rls == "outcome"])
	prd <- names(rls[rls == "exposure"])

	# Name/term splits
	nms <-
		strsplit(nm, "_") |>
		{\(.x) do.call(rbind, .x)}() |>
		data.frame()
	colnames(nms) <- c("name", ".id", "pattern")

	# Always broken into groups by term
	# y = outcome
	# x = exposure
	# m = mediator
	# p = predictor (confounder)

	nms$outcome <- substr(nms$.id, start = 1, stop = 2)
	nms$exposure <- substr(nms$.id, start = 3, stop = 4)
	nms$number <- 1:nrow(nms)

	# Rename the specific terms (if available)
	for (i in 1:nrow(nms)) {
		for (j in c("outcome", "exposure")) {
			y <- as.integer(substr(nms[[j]][i], start = 2, stop = 2))
			if (y == 0) {

				z <- NA
				# z <- names(rls)[rls == j]
				# if (length(z) == 0 | j != "covariate") {
				# 	z <- NA
				# } else {
				# 	z <- paste(z, collapse = ", ")
				# }
			} else if (y >= 1) {
				if (j == "exposure") {
					z <- names(rls)[rls == j][y]
				} else {
					z <- names(rls)[rls == j][y]
				}
			}

			nms[[j]][i] <- z
		}
	}

	nms[names(nms) == ".id"] <- nm

	# Add in model list
	nms$models <- x

	# Cleans up final table after merging in formulas
	subset(nms, select = c(name, number, outcome, exposure, models))

}
