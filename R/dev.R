#' Reformulate a Hypothesis
#'
#' After `hypothesis` objects have been fitted in a `framework`, the generated list of models can be evaluated based on how the models were composed via the __combination__ argument.
#'
#' @return A `framework` object with an additional hypothesis
#'
#' @inheritParams add_hypothesis
#'
#' @export
reformulate <- function(framework, name) {

	# Start with tidy table for a hypothesis
	f <-
		framework[framework$name == name,] %>%
		tidyr::unnest(tidy) %>%
		dplyr::select(-c(name, fit, statistic))

	# Trimmed data set is no longer a framework object
	f
}

#' Tidy framework
#' @export
tidy.framework <- function(framework, name) {

	# Start with tidy table for a hypothesis
	f <-
		framework[framework$name == name,] %>%
		tidyr::unnest(tidy) %>%
		dplyr::select(-c(name, fit, statistic))

	# Trimmed data set is no longer a framework object
	f
}


#' Check for any significant relationships and return just those variables
#' @export
identify_significant_predictors <- function(x) {

	# Assumes table format, made via "direct" combination
	x <- x[x$p.value < 0.05 & x$term != "(Intercept)", ]

	# Predictors
	predictors <- x$term
	outcomes <- x$outcome

	# Create new formula
	f <-
		paste(predictors, collapse = " + ") %>%
		paste(outcomes, ., sep = " ~ ") %>%
		stats::as.formula(.)

	# Return formula
	f

}

#' Reduce full term formula to only significant variables
#' Takes a tidy tibble and returns only the parameters that are significant, excluding intercept.
find_significant_terms <- function(x, p.value = 0.05) {

	predictors <- x$term[x$p.value < p.value & x$term != "(Intercept)"]

	# Return terms
	predictors

}




#' Requires a tidy tibble from a hypothesis that has been built with the same
#' exposure and has been built in parallel. It finds which potential covariates
#' may be confounders.
find_parallel_confounders <- function(x, name) {
	y <- tidy.framework(x, name)
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

#' If its done in sequential adjustment, then its more complex.
find_sequential_confounders <- function(x, name) {

	y <- tidy.framework(x, name)
	y <- y[-c(match(c("std.error", "conf.low", "conf.high"), names(y)))]

	# Base effect
	exposure <- unique(y$exposure)
	outcome <- unique(y$outcome)

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
