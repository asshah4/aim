#' Carve Out Confounders
#'
#' @return A `hypothesis` object
#' @export
carve_out <- function(study, name, ...) {

	# Get variables
	combination <- fetch_combination(study, name)
	data <- fetch_data(study, name)
	test <- fetch_test(study, name)
	formulae <- fetch_formulae(study, name)

	switch(
		combination,
		sequential = {

			# In sequential models, care about the relationship between exposure and outcome
			# When each covariate is added, care about if it changes y ~ x relationship
			x <- extract_models(study, name)
			y <- x[x$term == x$exposure, ]

			# Look for change in effect size
			n <- nrow(y)
			f <- formulae[[n]]
			confounders <- labels(stats::terms(f))[-1]
			delta <- 0.10

			for (i in 2:n) {
				base_est <- y$estimate[y$number == (i - 1)]
				new_est <- y$estimate[y$number == i]
				delta_est <- abs(base_est - new_est) / base_est
				if (!(abs(delta_est) > delta)) {
					confounders[i - 1] <- NA
				}
			}

			# Remove NA variables, leaving behind only that which should be adjusted
			confounders <- na.omit(confounders)
		}
	)

}

