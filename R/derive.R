#' Derive Formula With Confounders
#'
#' @description
#' `r lifecycle::badge('experimental')`
#' Removes confounders from a hypothesis object based on different approaches.
#'
#'   * __sequential__ = The model is analyzed for each additional covariate
#'   added, looking for changes in effect size defined by the variable `delta`.
#'   If the change is significant, the covariate is maintained as a potential
#'   confounder, otherwise removed.
#'
#' @return Either a `study` object, invisibly, that has been updated with a modified
#'   `hypothesis` or the modified `hypothesis` object on their own in a list,
#'   set by the user.
#'
#' @param study A `study` object that has been constructed
#'
#' @param name Name of a an existing `hypothesis` object
#'
#' @param new_name Name of the new `hypothesis` that is created after checking
#'   for confounding. Defaults to modifying the original **name** by appending
#'   `*_cut`.
#'
#' @param delta Percent change to be used as cut-off for considering an
#'   important change in the outcome ~ exposure relationship. Defaults to
#'   **0.10** as supported by the epidemiology literature.
#'
#' @param return_study Logical option for returning an updated `study` object or a list of `hypothesis` object. Defaults to TRUE, indicating the study should be returned.
#'
#' @param ... For extensibility
#' @export
derive <- function(study,
									 name,
									 new_name = paste0(name, "_cut"),
									 delta = 0.10,
									 return_study = TRUE,
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

	switch(
		combination,
		sequential = {

			# In sequential models, y ~ x relations is what matters
			# Each covariate may change the y ~ x effect size
			m <-
				study$model_map %>%
				.[.$name == name,]
			x <- extract(study, name)

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

		}
	)

	# Return to confounders to the study
	attr(study, "var_table") <- vars

	# Return the study, if indicated
	if (return_study) {
		x <- vars[vars$name == name,]
		n <- nrow(x)
		for (i in 1:n) {

			f <-
				paste(x$confounders[[i]], collapse = " + ") %>%
				paste(paste0("X(", x$exposures[i], ")"), ., sep = " + ") %>%
				paste(x$outcomes[i], ., sep = " ~ ") %>%
				stats::formula()

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

			# Add back to study
			study <-
				study %>%
				draw(h, name = new_name)
		}

		# Return invisibly
		invisible(study)
	}
	# Else, create and return the new hypothesis object
	else {
		x <- vars[vars$name == name,]
		n <- nrow(x)
		hlist <- list()

		for (i in 1:n) {

			f <-
				paste(x$confounders[[i]], collapse = " + ") %>%
				paste(paste0("X(", x$exposures[i], ")"), ., sep = " + ") %>%
				paste(x$outcomes[i], ., sep = " ~ ") %>%
				stats::formula()

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

		# Return visibly
		return(hlist)
	}

}

