#' Carve Out Confounders
#'
#' @return A `hypothesis` object
#' @export
carve_out <- function(study, name, ...) {

	m <- study$model_map %>%
		.[.$name == name, ]
	p <- study$path_map %>%
		.[.$name == name, ]

	combination <- fetch_combination(study, name)
	data <- fetch_data(study, name)
	test <- fetch_test(study, name)
	formulae <- fetch_formulae(study, name)

	# In sequential models, care about the relationship between exposure and outcome
	# When each covariate is added, care about if it changes y ~ x relationship
	x <- extract_models(study, name)
	y <- x[x$term == x$exposure, ]

	t <- attr(study, "test_table")
	t$call

	# Look for change in effect size
	n <- nrow(y)
	confounders
	f <- tail(formulae, n = 1)[[1]]
	labels(stats::terms(f))
	delta <- 0.10

	for (i in 2:n) {
		base_est <- y$estimate[y$number == (i - 1)]
		new_est <- y$estimate[y$number == i]
		delta_est <- abs(base_est - new_est) / base_est
		if (abs(delta_est) > delta) {
			confounders[i - 1]
			x$term[x$number == i]
			confounders[i - 1] <- y$term[y$number == i & y$term == vars[i]]
		}
	}

}

