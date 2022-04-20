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
#' into a `forge`.
#'
#' @name forge
#' @importFrom dplyr mutate
#' @export
forge <- function(x, ...) {
  UseMethod("forge", object = x)
}

#' @rdname forge
#' @export
forge.model_archetype <- function(x,
                                  name = deparse1(substitute(x)),
                                  data = data.frame(),
                                  ...) {
  if (length(x) == 0) {
    return(new_forge())
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
    mutate(run = TRUE) |>
  	# Hash = name, type, subytype, formula
    mutate(hash = make_hash(x))


  # Core table
  tbl <- construct_model_table(
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
    run = y$run,
    hash = y$hash
  )

  # Additional attributes to be included in the forged object
  dl <- list()
  dl[[name]] <- data
  dl <- data_list(dl)

  pe <- parameter_estimates(x)
  mi <- model_estimates(x)
  names(pe) <- names(mi) <- tbl$hash

  # Make forge
  new_forge(
    x = tbl,
    data_list = dl,
    parameters = pe,
    model_info = mi
  )
}

#' @rdname forge
#' @export
forge.formula_archetype <- function(x,
                                    name = deparse1(substitute(x)),
                                    data = data.frame(),
                                    ...) {
  if (length(x) == 0) {
    return(new_forge())
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
      description = NA_character_
    ) |>
    mutate(run = FALSE) |>
  	# Hash = name, type, subytype, formul
    mutate(hash = make_hash(x, name))

  tbl <- construct_model_table(
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
    run = y$run,
    hash = y$hash
  )

  # Additional attributes to be included in the forged object
  dl <- list()
  dl[[name]] <- data
  dl <- data_list(dl)

  pe <- parameter_estimates()
  mi <- model_estimates()

  # Make forge
  new_forge(
    x = tbl,
    data_list = dl,
    parameters = pe,
    model_info = mi
  )
}

#' @rdname forge
#' @export
mdls <- forge

# Model Forge Construction and Definition --------------------------------------

#' Model forge construction
#' @keywords internal
#' @noRd
construct_model_table <- function(model = list(),
                                  type = character(),
                                  subtype = character(),
                                  name = character(),
                                  description = character(),
                                  formula = character(),
                                  outcome = character(),
                                  exposure = character(),
                                  mediator = character(),
                                  terms = list(),
                                  run = logical(),
                                  hash = character()) {

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
  vec_assert(terms, ptype = list())
  vec_assert(run, ptype = logical())
  vec_assert(hash, ptype = character())

  # Essentially each row is made or added here
  tbl <- tibble::tibble(
    model = model,
    type = type,
    subtype = subtype,
    name = name,
    description = description,
    formula = formula,
    outcome = outcome,
    exposure = exposure,
    mediator = mediator,
    terms = terms,
    run = run,
    hash = hash
  )

  # Return tibble
  tbl
}

#' Model forge initialization
#' @keywords internal
#' @noRd
new_forge <- function(x = tibble(),
                      data_list = data_list(),
                      parameters = parameter_estimates(),
                      model_info = model_estimates()) {

  # Validation
  stopifnot(is.data.frame(x))
  vec_assert(data_list, ptype = data_list()) # Acutally a list_of class
  vec_assert(parameters, ptype = parameter_estimates())
  vec_assert(model_info, ptype = model_estimates())

  tibble::new_tibble(
    x,
    data_list = data_list,
    parameters = parameters,
    model_info = model_info,
    class = "forge",
    nrow = nrow(x)
  )
}

#' @keywords internal
#' @noRd
methods::setOldClass(c("forge", "vctrs_vctr"))

# Output -----------------------------------------------------------------------

#' @export
print.forge <- function(x, ...) {
  cat(sprintf("<%s>\n", class(x)[[1]]))
  cli::cat_line(format(x)[-1])
}

#' @export
vec_ptype_full.forge <- function(x, ...) {
  "forge"
}

#' @export
vec_ptype_abbr.forge <- function(x, ...) {
  "mdls"
}


# Casting and coercion ---------------------------------------------------------

### MODEL FORGE

#' @export
forge_ptype2 <- function(x, y, ..., x_arg = "", y_arg = "") {
  out <- tib_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)

  # Combine new data
  dl <- c(attributes(x)$data_list, attributes(y)$data_list)
  pe <- c(attributes(x)$parameters, attributes(y)$parameters)
  mi <- c(attributes(x)$model_info, attributes(y)$model_info)

  new_forge(out, data_list = dl, parameters = pe, model_info = mi)
}

#' @export
forge_cast <- function(x, to, ..., x_arg = "", to_arg = "") {
  out <- tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)

  # Combine new data
  dl <- c(attributes(x)$data_list, attributes(to)$data_list)
  pe <- c(attributes(x)$parameters, attributes(y)$parameters)
  mi <- c(attributes(x)$model_info, attributes(to)$model_info)

  new_forge(out, data_list = dl, parameters = pe, model_info = mi)
}

#' @export
vec_ptype2.forge.forge <- function(x, y, ...) {
  forge_ptype2(x, y, ...)
}

#' @export
vec_cast.forge.forge <- function(x, to, ...) {
  forge_cast(x, to, ...)
}

### TIBBLE

#' @export
vec_ptype2.forge.tbl_df <- function(x, y, ...) {
  forge_ptype2(x, y, ...)
}

#' @export
vec_ptype2.tbl_df.forge <- function(x, y, ...) {
  forge_ptype2(x, y, ...)
}

#' @export
vec_cast.forge.tbl_df <- function(x, to, ...) {
  forge_cast(x, to, ...)
}

#' @export
vec_cast.tbl_df.forge <- function(x, to, ...) {
  tib_cast(x, to, ...)
}

### DATA.FRAME

#' @export
vec_ptype2.forge.data.frame <- function(x, y, ...) {
  forge_ptype2(x, y, ...)
}

#' @export
vec_ptype2.data.frame.forge <- function(x, y, ...) {
  forge_ptype2(x, y, ...)
}

#' @export
vec_cast.forge.data.frame <- function(x, to, ...) {
  forge_cast(x, to, ...)
}

#' @export
vec_cast.data.frame.forge <- function(x, to, ...) {
  df_cast(x, to, ...)
}
