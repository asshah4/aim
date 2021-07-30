#' Initialize a Hypothesis Framework
#'
#' @family frameworks
#' @export
framework <- function(...) {
	# Base structure is that of a tibble
	framework <- tibble::tribble(
		~name, ~outcome, ~exposure, ~formulae, ~fit, ~tidy
	)

	attr(framework, "data_table") <- tibble::tribble(
		~hypothesis_name, ~data_name, ~data, ~strata
	)

	attr(framework, "test_table") <- tibble::tribble(
		~hypothesis_name, ~test, ~test_opts
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
	parameters <- attributes(hypothesis)$parameters
	formulae <- attributes(hypothesis)$formulae
	n <- nrow(parameters)

	for (i in 1:n) {
		framework <- framework %>%
			tibble::add_row(
				name = name,
				outcome = parameters$outcomes[i],
				exposure = parameters$exposures[i],
				formulae = list(formulae[[i]])
			)
	}

	# Each of the rows of the hypothesis are sub-hypotheses to be tested
	if (length(attributes(hypothesis)$data) > 0) {
		framework <- .link_hypothesis_to_data(framework, hypothesis, name)
	}

	# Return
	framework
}

