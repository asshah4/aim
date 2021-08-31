#' Reduce full term formula to only significant variables
#' Takes a tidy tibble and returns only the parameters that are significant, excluding intercept.
find_significant_terms <- function(x, p.value = 0.05) {

	predictors <- x$term[x$p.value < p.value & x$term != "(Intercept)"]

	# Return terms
	predictors

}

#' Check For Confounding
#'
#' @export
check_confounding <- function(x, name) {

	### APPROACH

	# Takes hypothesis from study

	# Examines the combinations of exposures and outcomes to look for potential confounding

	# Should return a new hypothesis object that is added to the study

	# This new hypothesis should be with less variables (obviously), and can be then refitted

	# If confounders are already listed in the options, more "aggressive" analysis of confounders should be performed (e.g. formal confounding analyisis, such as with strata)


	# Trimmed data set is no longer a study object
	t <-
		x[x$name == name,] %>%
		tidyr::unnest(tidy) %>%
		dplyr::select(-c(name, fit, statistic))

	# Identify important attributes of the named hypothesis
	combination <- fetch_combination(x, name)

	# Need to establish outcomes and exposures
	groups <-
		expand.grid(outcome = t$outcome, exposures = t$exposure) %>%
		unique()

	for (o in groups$outcome) {
		for (e in groups$exposures) {
			switch(
				combination,
				sequential = {
					# Identify confounders
				 	confounders <-
				 		find_sequential_confounders(x, name, outcome = o, exposure = e)

				 	# Place back into study
				 	y <- x[x$name == name & x$outcome == o & x$exposure == e, ]
				 	attributes(y)$var_table

				}
			)
		}
	}



}


#' Requires a tidy tibble from a hypothesis that has been built with the same
#' exposure and has been built in parallel. It finds which potential covariates
#' may be confounders.
find_parallel_confounders <- function(x, name) {
	y <- tidy.study(x, name)
	y <- y[-c(match(c("std.error", "conf.low", "conf.high"), names(y)))]

	# Find base effect size
	test <- get_test(x, name)
	data <- get_data(x, name)
	exposure <- unique(y$exposure)
	outcome <- unique(y$outcome)
	formulae <- stats::formula(paste(outcome, exposure, sep = " ~ "))
	effect <- possible_fit(test, formulae, data) %>%
		broom::tidy(conf.int = TRUE, exponentiate = TRUE)
	base_est <- effect$estimate[effect$term == unique(y$exposure)]

	# Find the amount of change with adjustment for the exposure
	z <- subset(y, term == exposure)
	z$delta <- abs(z$estimate - base_est) / base_est

	# Select which variables have a >10% change
	numbers <- z$number[abs(z$delta) > 0.10]
	confounders <-
		y$term[y$number %in% numbers & !(y$term %in% c(exposure, "(Intercept)"))]

	# Return potential confounders
	confounders
}

#' Return the confounders determined in sequential models
find_sequential_confounders <- function(x, name, outcome, exposure) {

	y <-
		x[x$name == name & x$outcome == outcome & x$exposure == exposure, ] %>%
		tidyr::unnest(tidy) %>%
		dplyr::select(-c(name, fit, statistic, std.error))

	# Make list of terms
	vars <-
		y$term[y$term != "(Intercept)"] %>%
		unique()

	n <- length(vars)
	confounders <- list()

	for (i in 2:n) {
		base_est <- y$estimate[y$term == exposure & y$number == (i - 1)]
		new_est <- y$estimate[y$number == i & y$term == exposure]
		delta <- abs(base_est - new_est) / base_est
		if (abs(delta) > 0.10) {
			confounders[i - 1] <- y$term[y$number == i & y$term == vars[i]]
		}
	}

	# Return confounders
	unlist(confounders)

}
