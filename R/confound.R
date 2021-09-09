#' Derive Formula With Confounders
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Removes confounders from a hypothesis object based on different approaches.
#'
#'   * __sequential__ = The model is analyzed for each additional covariate
#'   added, looking for changes in effect size defined by the variable `delta`.
#'   If the change is significant, the covariate is maintained as a potential
#'   confounder, otherwise removed.
#'
#'   * __parallel__ = The parallel models are analyzed for the effect of each
#'   individual covariate that was tested against the _outcome ~ exposure_
#'   relationship. The change in effect size of that relationship is examined
#'   for being of size `delta`. If so, the covariate is retained as a potential
#'   confounder (or otherwise removed).
#'
#' @return A list of `hypothesis` objects that have been modified
#'
#' @param study A `study` object that has been constructed
#'
#' @param name Name of a an existing `hypothesis` object
#'
#' @param delta Percent change to be used as cut-off for considering an
#'   important change in the outcome ~ exposure relationship. Defaults to
#'   **0.10** as supported by the epidemiology literature.
#'
#' @param return_study Logical value, defaults to FALSE, on if the original
#'   `study` should be returned when this function is called. This is a
#'   developer feature, mainly for internal use (as is the function itself). It
#'   returns the modified hypotheses as a list in position `[[1]]`, and the
#'   updated study in position `[[2]]`.
#'
#' @param ... For extensibility
#'
#' @family confounders finders
#' @export
find_confounders <- function(study,
														 name,
														 delta = 0.10,
														 return_study = FALSE,
														 ...) {

	# Get variables
	combination 	<- fetch_combination(study, name)
	test					<- fetch_test(study, name)
	test_opts 		<- fetch_test_opts(study, name)
	data					<- fetch_data(study, name)
	data_name 		<- fetch_data_name(study, name)
	strata				<- fetch_strata(study, name)
	formulae			<- fetch_formulae(study, name)
	vars					<- attributes(study)$var_table

	# The test needs to have been run already for analysis
	check_hypothesis(study, name, run = TRUE)

	switch(
		combination,
		sequential = {

			# In sequential models, y ~ x relations is what matters
			# Each covariate may change the y ~ x effect size
			m <-
				study$model_map %>%
				.[.$name == name,]
			x <- extract_models(study, name)

			# Check for each unique outcome, and for each unique exposure combination
			out <- unique(x$outcomes)
			exp <- unique(x$exposures)

			for (i in out) {
				for (j in exp) {
					# Get table and variables of unique hypotheses
					y <- x[x$outcomes == i & x$term == j, ]
					n <- nrow(y)
					f <-
						m %>%
						.[.$outcomes == i & .$exposures == j, ] %>%
						.[.$number == max(.$number), ] %>%
						.$formulae %>%
						.[[1]]

					# Unrefined confounders
					confounders <- labels(stats::terms(f))[-1]

					# Look for change in effect size
					for (k in 2:n) {
						base_est <- y$estimate[y$number == (k - 1)]
						new_est <- y$estimate[y$number == k]
						delta_est <- abs(base_est - new_est) / base_est
						if (!(abs(delta_est) > delta)) {
							confounders[k - 1] <- NA
						}
					}

					# Remove NA variables, leaving only "true" confounders
					confounders <- list(stats::na.omit(confounders))

					# Update variable table to include confounders
					vars$confounders[vars$name == name &
													 	vars$outcomes == i &
													 	vars$exposures == j] <- confounders

				}
			}
		},
		parallel = {
			# In a parallel combination, the relationship should be y ~ x + c

			# Check for each unique outcome, and for each unique exposure combination
			m <-
				study$model_map %>%
				.[.$name == name,]
			x <- extract_models(study, name)
			out <- unique(x$outcomes)
			exp <- unique(x$exposures)

			for (i in out) {
				for (j in exp) {

					# Need base estimate relationsihp
					f <- stats::formula(paste(i, j, sep = " ~ "))
					effect <-
						fit_parsnip_models(list(f), test, .data = data) %>%
						tidy_tests() %>%
						.[[1]]
					base_est <- effect$estimate[effect$term == j]

					# Subset of models for comparison
					y <- x[x$outcomes == i & x$exposures == j, ]
					z <- subset(y, term == j)
					z$delta <- abs((base_est - z$estimate) / base_est)
					n <- z$number[z$delta > delta]

					confounders <-
						y %>%
						.[.$number %in% n, ] %>%
						.[!(.$term %in% c("(Intercept)", j)), ] %>%
						.$term

					# Update variable table to include confounders
					vars$confounders[vars$name == name &
													 	vars$outcomes == i &
													 	vars$exposures == j] <- list(confounders)

				}
			}
		}
	)

	# Update study (may not be returned unless requested)
	attr(study, "var_table") <- vars

	# Create and return a list of hypothesis objects
	x <- vars[vars$name == name,]
	n <- nrow(x)
	hlist <- list()

	for (i in 1:n) {

		# Need to handle NA/missing objects
		if (length(x$confounders[[i]]) > 0) {
			f <-
				paste(x$confounders[[i]], collapse = " + ") %>%
				paste(paste0("X(", x$exposures[[i]], ")"), ., sep = " + ") %>%
				paste(x$outcomes[i], ., sep = " ~ ") %>%
				stats::formula()
		} else {
			f <-
				paste0("X(", x$exposures[[i]], ")") %>%
				paste(x$outcomes[i], ., sep = " ~ ") %>%
				stats::formula()
		}

		# Create new hypothesis object
		h <- new_hypothesis(
			hypothesis = f,
			combination = "direct",
			test = test,
			test_opts = test_opts,
			data = data,
			data_name = data_name,
			strata = strata,
			origin = name
		)

		validate_hypothesis(h)

		# Add to list
		hlist[[i]] <- h

	}

	if (return_study) {
		list(hlist, study)
	} else {
		hlist
	}

}


#' Reconstruct a Hypothesis
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' Modify a `hypothesis` within a study with different approaches.
#'
#' @return A `study` object
#'
#' @inheritParams find_confounders
#'
#' @param new_name Name of the new `hypothesis` that is created after
#'   modification. Defaults to modifying the original __name__ by appending
#'   `*_cut`.
#'
#' @param approach String that informs _how_ to reconstruct the
#'   hypothesis. Options include:
#'
#'   * __confounding__ = Check for relevant terms based on
#'   [dagger::find_confounders()]. This includes the __delta__ argument to
#'   set the threshold for changes in effect size that are considered relevant.
#'
#' @param ... Additional, optional parameters based on approach being used
#' @family confounders studies
#' @export
reconstruct <- function(study,
												name,
												new_name = paste0(name, "_cut"),
												approach = "confounding",
												...) {


	switch(
		approach,
		confounding = {
			x <- find_confounders(study, name, return_study = TRUE)
			hlist <- x[[1]]
			study <- x[[2]]

			# Add back to study
			for (i in 1:length(hlist)) {
				study <-
					study %>%
					add_hypothesis(hlist[[i]], name = new_name)
			}
		}
	)

	# Fit updated models
	study <- construct_map(study)

	# Return
	invisible(study)
}
