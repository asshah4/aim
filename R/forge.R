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
#' @name forge
#' @importFrom dplyr mutate
#' @export
model_forge <- function(x, ...) {
	UseMethod("model_forge", object = x)
}

#' @rdname forge
#' @export
model_forge.model_archetype <- function(x,
																				data = data.frame(),
																				...) {
  if (length(x) == 0) {
    return(new_model_forge())
  }

  fl <- field(x, "fmls")

  # Expand components into appropriate table
  y <-
  	x |>
    vec_data() |>
    tibble() |>
    # Add in formula components and corresponding roles
    dplyr::bind_cols(vec_data(fl)) |>
    dplyr::rowwise() |>
    mutate(across(
      c(outcome, exposure, mediator, strata),
      function(.x) {
        t <- .x
        if (length(t) == 0) {
          t <- NA_character_
        } else {
          t <- as.character(t)
        }
        t
      }
    )) |>
  	mutate(terms = list(get_terms(fmls))) |>
    dplyr::ungroup() |>
    mutate(tidy = possible_tidy(x)) |>
    mutate(glance = possible_glance(x)) |>
    mutate(observations = sapply(glance, FUN = function(.x) {
      .x$nobs
    })) |>
  	mutate(run = TRUE)

  new_model_forge(
    model = y$model,
    type = y$type,
    subtype = y$subtype,
    name = y$name,
    description = y$description,
    formula = y$formula,
    outcome = y$outcome,
    exposure = y$exposure,
    mediator = y$mediator,
    terms = y$terms,
    observations = y$observations,
    glance = y$glance,
    tidy = y$tidy,
    run = y$run,
    data = data
  )
}

#' @rdname forge
#' @export
model_forge.formula_archetype <- function(x,
																					name = deparse1(substitute(x)),
																					...) {

  if (length(x) == 0) {
    return(new_model_forge())
  }

	# Ensure appropriate formula can be modeled later if need be
	f <- x[field(x, "order") == 2]

  # Expand formula into appropriate table
	y <-
		f |>
		vec_data() |>
    dplyr::bind_cols(fmls = f) |>
    tibble() |>
		dplyr::mutate(name = name) |>
    dplyr::rowwise() |>
    mutate(across(
      c(outcome, exposure, mediator, strata),
      function(.x) {
        t <- .x
        if (length(t) == 0) {
          t <- NA_character_
        } else {
          t <- as.character(t)
        }
        t
      }
    )) |>
  	mutate(terms = list(get_terms(fmls))) |>
    dplyr::ungroup() |>
		# These items would need a model to be included
		mutate(
			model = list(NA),
			type = NA_character_,
			subtype = NA_character_,
			description = NA_character_,
			tidy = list(NA),
			glance = list(NA),
			observations = NA_integer_,
		) |>
  	mutate(run = FALSE)

  new_model_forge(
    model = y$model,
    type = y$type,
    subtype = y$subtype,
    name = y$name,
    description = y$description,
    formula = y$formula,
    outcome = y$outcome,
    exposure = y$exposure,
    mediator = y$mediator,
    terms = y$terms,
    observations = y$observations,
    glance = y$glance,
    tidy = y$tidy,
    run = y$run
  )
}

#' @rdname forge
#' @export
mdls <- model_forge

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
                            observations = integer(),
														terms = list(),
                            tidy = list(),
                            glance = list(),
                            run = logical(),
														data = data.frame(),
														data_list = list()) {

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
  vec_assert(observations, ptype = integer())
  vec_assert(terms, ptype = list())
  vec_assert(tidy, ptype = list())
  vec_assert(glance, ptype = list())
  vec_assert(run, ptype = logical())
  vec_assert(data, ptype = data.frame())
  vec_assert(data_list, ptype = list())

  # Clean up data
  data_list[[name]] <- data

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
    observations = observations,
    terms = terms,
    tidy = tidy,
    glance = glance,
    run = run
  )

  # Validation
  stopifnot(is.data.frame(x))

  tibble::new_tibble(
    x,
    class = "model_forge",
    data_list = data_list,
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

### MODEL FORGE

#' @export
mdls_ptype2 <- function(x, y, ..., x_arg = "", y_arg = "") {
  out <- tib_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)

  # Data from both components
  data_x <- attr(x)$data_list
  data_to <- attr(to)$data_list

  # Combine new data
  dl <- unique(c(data_x, data_to))

  new_model_forge(out, data_list = dl)

}

#' @export
mdls_cast <- function(x, to, ..., x_arg = "", to_arg = "") {
  out <- tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)

  # Data from both components
  data_x <- attr(x)$data_list
  data_to <- attr(to)$data_list

  # Combine new data
  dl <- unique(c(data_x, data_to))

  new_model_forge(out, data_list = dl)

}

#' @export
vec_ptype2.model_forge.model_forge <- function(x, y, ...) {
  mdls_ptype2(x, y, ...)
}

#' @export
vec_cast.model_forge.model_forge <- function(x, to, ...) {
  mdls_cast(x, to, ...)
}

### TIBBLE

#' @export
vec_ptype2.model_forge.tbl_df <- function(x, y, ...) {
	mdls_ptype2(x, y, ...)
}

#' @export
vec_ptype2.tbl_df.model_forge <- function(x, y, ...) {
	mdls_ptype2(x, y, ...)
}

#' @export
vec_cast.model_forge.tbl_df <- function(x, to, ...) {
	mdls_cast(x, to, ...)
}

#' @export
vec_cast.tbl_df.model_forge <- function(x, to, ...) {
	tib_cast(x, to, ...)
}

### DATA.FRAME

#' @export
vec_ptype2.model_forge.data.frame <- function(x, y, ...) {
	mdls_ptype2(x, y, ...)
}

#' @export
vec_ptype2.data.frame.model_forge <- function(x, y, ...) {
	mdls_ptype2(x, y, ...)
}

#' @export
vec_cast.model_forge.data.frame <- function(x, to, ...) {
	mdls_cast(x, to, ...)
}

#' @export
vec_cast.data.frame.model_forge <- function(x, to, ...) {
	df_cast(x, to, ...)
}

# Data Addition ----------------------------------------------------------------

