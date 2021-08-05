#' Reformulate a Hypothesis
#'
#' After `hypothesis` objects have been fitted in a `framework`, the generated list of models can be evaluated based on how the models were composed via the __combination__ argument.
#'
#' @return A `framework` object with an additional hypothesis
#'
#' @inheritParams add_hypothesis
#'
#' @export
reformulate <- function(framework, name, data_name = NULL) {

	# Attributes of the specific hypothesis are needed
	f <-
		framework[framework$name == name,] %>%
		{
			if (!is.null(data_name)) {
				.[.$data_name == data_name,]
			}
		}

	opts <-

	# Simplify the framework based on the hypothesis and dataset used
	if (is.null(data_name)) {
		f <-
			framework[framework$name == name,] %>%
			tidyr::unnest(tidy) %>%
			dplyr::select(-c(name, fit, data_name, statistic))
	} else {
		f <-
			framework[framework$name == name & framework$data_name == data_name, ] %>%
			tidyr::unnest(tidy) %>%
			dplyr::select(-c(name, fit, data_name, statistic))
	}

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
find_significant_terms <- function(x) {

	predictors <- x$term[x$p.value < 0.05 & x$term != "(Intercept)"]

	# Return terms
	predictors

}

#' Takes a hypothesis list with fit models and applies functions
