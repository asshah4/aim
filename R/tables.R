#' @export
explode.list_of_models <- function(x, ...) {

	# Basic extraction
	nm <- names(x)
	labs <- labels(x)

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
	tbl <-
		subset(nms, select = c(name, number, outcome, exposure, models))

	# Modify the class of this to be able to add and retrieve labels
	tbl <- structure(tbl,
									 class = c("mdls", class(tbl)),
									 labels = labs)

	# Return with labs attached
	tbl

}
