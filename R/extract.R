#' Extract models from a study
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' This function serves as a non-pipeable extraction function to obtain
#' aggregate components from a `model_map` object. Its designed to have a simple
#' call interface to allow for retrieval of model objects to be formatted and
#' displayed.
#'
#' @return Returns either a `model_map` object or a `tibble` based on the
#'   __flat__ argument
#'
#' @param model_map A `model_map` object that contains `hypothesis` objects that have
#'   been run
#'
#' @param which_ones Vector of names of hypotheses that should be extracted and
#'   distilled. Will only be applied to models that have been fit. Defaults to
#'   NULL, for which all tests will be returned.
#'
#' @param how A character string that describes how the test results should be
#'   returned. The choices are as follows:
#'
#'   * __fit__ = unmodified, raw model or test fit, which is the default option
#'
#'   * __tidy__ = tidy table of parameter estimates, confidence interval, and
#'   test statistics, using the [broom::tidy()] approach for the exported method
#'
#'   * __glance__ = a single-row summary of overall model or test as defined by
#'   the exported method from [broom::tidy()]
#'
#' @param flat Logical value for if the findings should be returned as a
#'   simple, unnested table or if the extracted values should be returned in a
#'   column as part of the original `model_map` object. Defaults to FALSE. This
#'   allows the function to more commonly be used within a pipe.
#'
#' @param ... For additional parameters based on above method listed under the
#'   __how__ parameter, such as `conf.level = 0.95` to be passed along when
#'   performing a tidying function
#'
#' @family extractors
#' @export
extract_results <- function(model_map,
														which_ones = NULL,
														how = "fit",
														flat = FALSE,
														...) {

	# Validate
	validate_class(model_map, "model_map")
	validate_stage(model_map, "run")

	# Additional arguments
	dots <- list(...)
	status <- attr(model_map, "status_table")

	# Select out models that have not yet been run
	# If specified hypothesis are named, force them to be re-run
	if (is.null(which_ones)) {
		x <-
			attributes(model_map)$status_table[c("name", "run")] %>%
			subset(., run == TRUE)
	} else {
		x <-
			attributes(model_map)$status_table[c("name", "run")] %>%
			subset(., name %in% which_ones) %>%
			subset(., run == TRUE)
	}

	# Declare which models will be extracted
	message(
		"The following hypotheses will be returned using the `",
		how,
		"` approach: ",
		"\n",
		paste(paste("-", x$name), collapse = "\n")
	)

	# Find out values based on how it should be approached
	switch(how,
				 fit = {
				 	res <-
				 		model_map %>%
				 		.[.$name %in% x$name,]

				 	# Early return
				 	return(res)
				 },
				 tidy = {
				 	# Defaults
				 	if ("conf.level" %in% names(dots)) {
				 		conf.level <- dots$conf.level
				 	} else {
				 		conf.level <- 0.95
				 	}
				 	if ("conf.int" %in% names(dots)) {
				 		conf.int <- dots$conf.int
				 	} else {
				 		conf.int <- TRUE
				 	}
				 	if ("exponentiate" %in% names(dots)) {
				 		exponentiate <- dots$exponentiate
				 	} else {
				 		exponentiate <- TRUE
				 	}

				 	res <-
				 		model_map %>%
				 		.[.$name %in% x$name,] %>%
				 		mutate(tidy = purrr::map(
				 			fit,
				 			~ broom::tidy(
				 				.x,
				 				conf.int = conf.int,
				 				conf.level = conf.level,
				 				exponentiate = exponentiate
				 			)
				 		))

				 	# Status update
				 	status$tidy[status$name %in% x$name] <- TRUE
				 },
				 glance = {
				 	res <-
				 		model_map %>%
				 		.[.$name %in% x$name,] %>%
				 		mutate(glance = purrr::map(fit, ~ broom::glance(.x)))

				 	# Status update
				 	status$tidy[status$name %in% x$name] <- TRUE
				 })

	# Update original status
	attr(model_map, "status_table") <- status

	# Return
	if (flat) {
		# Strip of model_map attributes
		flatten(res, check = FALSE)
	} else {
		# Place within original model_map
		model_map %>%
			.[!(.$name %in% x$name),] %>%
			dplyr::bind_rows(., res)
	}

}
