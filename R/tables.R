#' @export
explode.list_of_models <- function(x, ...) {

	nm <- names(x)
	rls <- roles(x)
	labs <- labels(x)

	# Group and covariate management
	grps <- groups(x)

	if (length(grps) > 0) {
		grp_nms <- unlist(unique(grps))
		g <- list()
		for (i in grp_nms) {
			g <-
				names(grps)[grps == i] |>
				{
					\(.x) paste(.x, collapse = ", ")
				}() |>
				{
					\(.x) append(g, .x)
				}()
		}

		cov <-
			names(rls)[rls == "covariate"] |>
			{
				\(.x) .x[!(.x %in% names(grps))]
			}() |>
			{
				\(.x) append(g, .x)
			}()

	} else {
		cov <- as.list(names(rls)[rls == "covariate"])
	}

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
	nms$covariate <- substr(nms$.id, start = 5, stop = 6)
	nms$mediator <- substr(nms$.id, start = 7, stop = 8)

	# Rename the specific terms (if available)
	for (i in 1:nrow(nms)) {
		for (j in c("outcome", "exposure", "covariate", "mediator")) {
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
				if (j == "covariate") {
					z <- cov[[y]]
				} else {
					z <- names(rls)[rls == j][y]
				}
			}

			nms[[j]][i] <- z
		}
	}

	nms[names(nms) == ".id"] <- nm


	# Cleans up final table after merging in formulas
	tbl <-
		list_to_table(x, id = ".id", val = "formula") |>
		merge(nms, by = ".id", sort = FALSE)

	# Return
	tbl |>
		subset(select = c(name,
											pattern,
											outcome,
											exposure,
											covariate,
											mediator,
											formula))

}
