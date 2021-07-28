#' Initialize a Hypothesis Framework
#'
#' @family frameworks
#' @export
framework <- function(...) {
	# Base structure is that of a tibble
	framework <- tibble::tribble(
		~name, ~outcome, ~exposure, ~formula, ~fit, ~tidy
	)

	attr(framework, "data") <- tibble::tribble(
		~hypothesis_name, ~data_name, ~data
	)

	# Return
	framework <-
		structure(framework, class = c("framework", class(framework)))
}

#' Add a Hypothesis to the Framework
#'
#' @family frameworks
#' @export
add_hypothesis <- function(framework,
													 hypothesis,
													 name = deparse(substitute(hypothesis)),
													 ...) {
	# Identify number of sub-hypotheses
	parameters <- attributes(hypothesis)$terms
	formulas <- attributes(hypothesis)$formulas[[1]]
	n <- nrow(parameters)

	for (i in 1:n) {
		framework <- framework %>%
			tibble::add_row(
				name = name,
				outcome = parameters$outcomes[i],
				exposure = parameters$exposures[i],
				formula = list(formulas[[i]])
			)
	}

	# Each of the rows of the hypothesis are sub-hypotheses to be tested
	framework <- tie_hypothesis_to_data(framework, hypothesis, name)

	# Return
	framework
}

#' Tie Hypothesis to Data
#'
#' Sets the status of the `framework` object to tie the hypothesis to the data it will be tested against.
#' @family internals
#' @export
tie_hypothesis_to_data <- function(framework, hypothesis, name) {

	# If data is present in the hypothesis
	attributes(framework)$data <- attributes(framework)$data %>%
		tibble::add_row(
			hypothesis_name = name,
			data_name = names(attributes(hypothesis)$data),
			data = attributes(hypothesis)$data
		)

	# Return framework
	framework
}
