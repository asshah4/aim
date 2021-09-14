#' Add a Hypothesis for a Model Map
#'
#' @description
#'
#' `r lifecycle::badge('stable')`
#'
#' Takes a `hypothesis` object and adds it to a `model_map`, allowing multiple
#' potential hypotheses to be put together for eventual analysis and comparison.
#' This involves the deconstructing of the hypothesis into its individual parts
#' and recomposition of the theorized formula.
#'
#' @return A `model_map` object that has had a `hypothesis` added
#'
#' @param model_map Object of `model_map` class
#'
#' @param hypothesis Object of `hypothesis` class
#'
#' @param name Name of the `hypothesis` object, which defaults to the name of
#'   the `hypothesis` object itself
#'
#' @param ... For extensibility
#'
#' @family hypotheses maps
#' @export
add_hypothesis <- function(model_map,
													 hypothesis,
													 name = deparse(substitute(hypothesis)),
													 ...) {

	validate_class(model_map, "model_map")

	# The hypothesis should be broken down to be incorporated into the map
	model_map <-
		model_map %>%
		.integrate_formula(hypothesis, name) %>%
		.integrate_data(hypothesis, name) %>%
		.integrate_test(hypothesis, name) %>%
		.integrate_status(
			hypothesis,
			name = name,
			run = FALSE,
			origin = attr(hypothesis, "origin")
		)

	# Updates to strata
	model_map <- .revise_strata(model_map, hypothesis, name)

	# Return
	model_map

}

#' Hypothesis Decomposers to Integrate into Model Map
#'
#' Break apart a `hypothesis` and add its components into the appropriate part of a `model_map` object.
#'
#' @return A `model_map` object
#' @inheritParams add_hypothesis
#' @name modify_map
#' @family internals
#' @keywords internal
NULL

#' @rdname modify_map
.integrate_formula <- function(model_map, hypothesis, name) {

	f <- stats::formula(stats::terms(hypothesis))
	comb <- attr(hypothesis, "combination")
	pars <- expand_formula(f, comb, table = TRUE)
	n <- nrow(pars)

	# Major parameters should be passed along
	for (i in 1:n) {
		model_map <-
			model_map %>%
			tibble::add_row(
				name = name,
				number = pars$number[i],
				outcome = pars$outcomes[i],
				exposure = pars$exposures[i],
				formulae = pars$formulae[i]
			) %>%
			unique()

		# Store the key variables
		attributes(model_map)$relation_table <-
			attributes(model_map)$relation_table %>%
			tibble::add_row(
				name = name,
				outcome = pars$outcomes[i],
				exposure = pars$exposures[i],
			) %>%
			unique()
	}

	# Return
	invisible(model_map)

}

#' @rdname modify_map
.integrate_data <- function(model_map, hypothesis, name) {

	# Data table for linking hypothesis to data
	attributes(model_map)$data_table <-
		attributes(model_map)$data_table %>%
		tibble::add_row(
			name = name,
			data_name = attributes(hypothesis)$data_name,
			strata = attributes(hypothesis)$strata
		) %>%
		unique()

	# Data list (to minimize too many data sets)
	attributes(model_map)$data_list <-
		attributes(model_map)$data_list %>%
		tibble::add_row(
			data_name = attributes(hypothesis)$data_name,
			data = list(attributes(hypothesis)$data)
		) %>%
		unique()

	# Return
	invisible(model_map)

}

#' @rdname modify_map
.integrate_test <- function(model_map, hypothesis, name) {

	attr(model_map, "test_table") <-
		attr(model_map, "test_table") %>%
		tibble::add_row(
			name = name,
			call = list(stats::formula(stats::terms(hypothesis))),
			test = list(attributes(hypothesis)$test),
			test_opts = attributes(hypothesis)$test_opts,
			combination = attributes(hypothesis)$combination,
			type = utils::tail(class(attributes(hypothesis)$test), 1)
		) %>%
		unique()

	# Return
	invisible(model_map)
}


#' @rdname modify_map
.integrate_status <- function(model_map,
															hypothesis,
															name,
															run = FALSE,
															error = FALSE,
															origin = NA) {

	# Modify the Status Table
	attributes(model_map)$status_table <-
		attributes(model_map)$status_table %>%
		tibble::add_row(
			name = name,
			run = run,
			error = error,
			origin = origin
		) %>%
		unique()

	# Return model_map
	invisible(model_map)

}

#' @rdname modify_map
.revise_strata <- function(model_map, hypothesis, name) {

	# Get basic variables
	strata <- fetch_strata(model_map, name)
	data <- fetch_data(model_map, name)

	# Remove original hypothesis from model map for later row binding
	m <- model_map
	x <- m[m$name == name, ]
	m <- m[m$name != name, ]

	# If strata are available
	if (!is.na(strata)) {
		level <-
			data[[strata]] %>%
			factor() %>%
			levels()

		xlist <- list()

		for (i in 1:length(level)) {
			xlist[[i]] <- x
		}

		y <- do.call("rbind", xlist)
		y$level <- rep(level, each = nrow(x))
	} else {
		y <- x
		y$level <- NA
	}

	z <-
		rbind(m, y) %>%
		dplyr::arrange(name)
	model_map <- z

	# Return
	invisible(model_map)
}

