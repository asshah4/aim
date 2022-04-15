# Model Forge ------------------------------------------------------------------

#' Forge Models into a Table
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' This function introduces a super class that combines both the `list` class
#' (and its derivative `list_of`) and regression models and/or hypothesis tests.
#' Models that are similar and share certain properties can be combined together
#' into a `model_forge`.
#'
#' @name model_forge
#' @export
model_forge <- function(x, ...) {

	if (validate_empty(x)) {
		return(new_model_forge())
	}

  validate_class(x, "model_archetype")

	for (i in seq_along(s)) {

		# First term and formula to understand the model
		s <- field(x[i], "script")
		t <- field(s, "terms")[[1]]
		f <- fmls(t, order = 1:4)[1]

	}

  # Scripts and formulas
	s <- field(x, "script")

	# Terms
	tl <- term_archetype()
	for (i in seq_along(s)) {
		t <- field(s[i], "terms")[[1]]
		tl <- append(tl, t)
	}
	tl <- unique(tl)

  # Create table of model data
  mad <-
    vec_data(x) |>
    tibble::tibble() |>
		dplyr::bind_cols(formula = vec_data(field(s, "formula"))) |>
  	dplyr::rowwise() |>
  	dplyr::mutate(outcome = {
			t <-
				field(script, "terms")[[1]] |>
  			vec_data() |>
  			{\(.x) .x$terms[.x$role %in% c("outcome", "dependent")]}()
  	}) |>
  	dplyr::mutate(exposure = {
			t <-
				field(script, "terms")[[1]] |>
  			vec_data() |>
  			{\(.x) .x$terms[.x$role == "exposure"]}()

			ifelse(length(t) == 0, NA, t)
  	}) |>
  	dplyr::mutate(mediator = {
			t <-
				field(script, "terms")[[1]] |>
  			vec_data() |>
  			{\(.x) .x$terms[.x$role == "mediator"]}()

			ifelse(length(t) == 0, NA, t)
  	}) |>
  	#dplyr::mutate(predictors = length(predictor)) |>
  	dplyr::ungroup() |>
  	dplyr::mutate(tidy = possible_tidy(x)) |>
  	dplyr::mutate(glance = possible_glance(x)) |>
  	dplyr::mutate(nobs = sapply(glance, FUN = function(.x) {.x$nobs}))

	new_model_forge(
		model = model,
		type = type,
		subtype = subtype,
		name = name,
		description = description,
		formula = formula,
		outcome = outcome,
		exposure = exposure,
		mediator = mediator,
		predictors = predictors,
		observations = observations,
		glance = glance,
		tidy = tidy,
		run = run,
		status = status
	)

}

#' @rdname model_forge
#' @export
mdls = model_forge

# Vector List ------------------------------------------------------------------

#' Formula list
#' @keywords internal
#' @noRd
new_model_forge <- function(model = list(),
														type = character(),
														subtype = character(),
														name = character(),
														description = character(),
														formula = character(),
														outcome = character(),
														exposure = character(),
														mediator = character(),
														predictors = numeric(),
														observations = numeric(),
														glance = tibble(),
														tidy = tibble(),
														run = logical(),
														status = list()) {

	# Validation
	vec_assert(model, ptype = list())
	vec_assert(type, ptype = character())
	vec_assert(subtype, ptype = character())
	vec_assert(name, ptype = character())
	vec_assert(description, ptype = character())
	vec_assert(formula, ptype = character())
	vec_assert(outcome, ptype = character())
	vec_assert(exposure, ptype = character())
	vec_assert(mediator, ptype = character())
	vec_assert(predictors, ptype = numeric())
	vec_assert(observations, ptype = numeric())
	vec_assert(glance, ptype = tibble())
	vec_assert(tidy, ptype = tibble())
	vec_assert(run, ptype = logical())
	vec_assert(status, ptype = list())

	# Essentially each row is made or added here
	x <- tibble::tibble(
		model = model,
		type = type,
		subtype = subtype,
		name = name,
		description = description,
		formula = formula,
		outcome = outcome,
		exposure = exposure,
		mediator = mediator,
		predictors = predictors,
		observations = observations,
		glance = glance,
		tidy = tidy,
		run = run,
		status = status
	)

	# Validation
  stopifnot(is.data.frame(x))

  tibble::new_tibble(
    x,
    class = "model_forge",
    nrow = nrow(x)
  )
}

#' @keywords internal
#' @noRd
methods::setOldClass(c("model_forge", "vctrs_vctr"))

# Output -----------------------------------------------------------------------

#' @export
print.model_forge <- function(x, ...) {
	cat(sprintf("<%s>\n", class(x)[[1]]))
	cli::cat_line(format(x)[-1])
}

#' @export
vec_ptype_full.model_forge <- function(x, ...) {
  "model_forge"
}

#' @export
vec_ptype_abbr.model_forge <- function(x, ...) {
  "mdls"
}


# Casting and coercion ---------------------------------------------------------

#' @export
mdls_ptype2 <- function(x, y, ..., x_arg = "", y_arg = "") {
  tib_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
}

#' @export
mdls_cast <- function(x, to, ..., x_arg = "", to_arg = "") {
  tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}

#' @export
vec_ptype2.model_forge.model_forge <- function(x, y, ...) {
  mdls_ptype2(x, y, ...)
}

#' @export
vec_cast.model_forge.model_forge <- function(x, to, ...) {
  mdls_cast(x, to, ...)
}
