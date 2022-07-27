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
forge <- function(..., data = NULL) {

  # Break early
  if (missing(..1)) {
    return(new_forge())
  }

  # Get arguments and hammer them flat (retaining the names if needed)
  args <- list(...)
  mc <- match.call()
  nms <- as.character(mc)[-1]
  nms[names(args) != ""] <- names(args)[names(args) != ""]

  # Flatten arguments if possible to a simple list
  mtl <- hammer(args, nms)
  nms <- names(mtl)

  # Now, everything should be an archetype object
  # Will need to "squish together" multiple tibbles for this
  tbl <- tibble()
  for (i in seq_along(mtl)) {
    x <- mtl[[i]]

    # Models
    if (class(x)[1] == "model_archetype") {
      fl <- field(x, "fmls")
      mi <- model_info(x)
      pe <- parameter_estimates(x)
      if (is.na(field(x, "strata_info"))) {
        si <- list(NA, NA, NA)
      } else {
        si <- as.formula(field(x, "strata_info"))
      }

      y <-
        x |>
        vec_data() |>
        tibble() |>
        # Add in formula components and corresponding roles
        dplyr::bind_cols(vec_data(fl)) |>
        dplyr::mutate(run = TRUE) |>
        dplyr::mutate(subname = dplyr::if_else(grepl(" ", name),
                                        gsub(" ", "", substr(name, 0, 7)),
                                        name)) |>
        dplyr::mutate(name = nms[i]) |>
        dplyr::mutate(strata = as.character(si[[2]])) |>
        dplyr::mutate(level = as.character(si[[3]])) |>
        dplyr::select(-strata_info)
    }

    # Formulas
    if (class(x)[1] == "formula_archetype") {
      # Ensure appropriate formula can be modeled later if need be
      f <- x[field(x, "order") %in% 1:3]

      # Formula hasn't been fit, so empty parameters
      pe <- parameter_estimates()
      mi <- model_info()

      # Expand formula into appropriate table
      y <-
        f |>
        vec_data() |>
        dplyr::bind_cols(fmls = f) |>
        tibble() |>
        # These items would need a model to be included
        mutate(
          model = list(NA),
          type = NA_character_,
          subtype = NA_character_,
          subname = NA_character_,
          description = NA_character_,
          level = NA,
          run = FALSE
        ) |>
        dplyr::mutate(name = nms[i])
    }

    # Common elements
    z <-
      y |>
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
      # Generate list of terms for each row
      mutate(terms = list(get_terms(fmls))) |>
      dplyr::ungroup() |>
      mutate(terms = term_list(terms))

    # Make individual row for a table
    tbl_row <- construct_model_table(
      model = z$model,
      type = z$type,
      subtype = z$subtype,
      name = z$name,
      subname = z$subname,
      number = z$number,
      description = z$description,
      formula = z$formula,
      outcome = z$outcome,
      exposure = z$exposure,
      mediator = z$mediator,
      strata = z$strata,
      level = z$level,
      terms = z$terms,
      model_info = mi,
      parameter_estimates = pe,
      run = z$run
    )

    tbl <- dplyr::bind_rows(tbl, tbl_row)
  }

  # If data is given, then it should be tied to the object as well
  dl <- list()
  if (!is.null(data)) {
    nm <- deparse1(substitute(data))
    dl[[nm]] <- data
  }

  # Into the model forge
  new_forge(
    x = tbl,
    data_list = data_list(dl)
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
                                  subname = character(),
                                  number = integer(),
                                  description = character(),
                                  formula = character(),
                                  outcome = character(),
                                  exposure = character(),
                                  mediator = character(),
                                  strata = character(),
                                  level = numeric(),
                                  terms = term_list(),
                                  model_info = model_info(),
                                  parameter_estimates = parameter_estimates(),
                                  run = logical()) {

  # Validation
  vec_assert(model, ptype = list())
  vec_assert(type, ptype = character())
  vec_assert(subtype, ptype = character())
  vec_assert(name, ptype = character())
  vec_assert(subname, ptype = character())
  vec_assert(number, ptype = integer())
  vec_assert(description, ptype = character())
  vec_assert(formula, ptype = character())
  vec_assert(outcome, ptype = character())
  vec_assert(exposure, ptype = character())
  vec_assert(mediator, ptype = character())
  vec_assert(strata, ptype = character())
  vec_assert(level, size = 1)
  vec_assert(terms, ptype = term_list())
  vec_assert(parameter_estimates, ptype = parameter_estimates())
  vec_assert(model_info, ptype = model_info())
  vec_assert(run, ptype = logical())

  # Handle emptiness
  if (length(parameter_estimates) == 0) {
    parameter_estimates <- NA
  }
  if (length(model_info) == 0) {
    model_info <- NA
  }
  if (length(strata) == 0) {
    strata <- NA
  }
  if (length(level) == 0) {
    level <- NA
  }

  # Essentially each row is made or added here
  tbl <- tibble::tibble(
    model = model,
    type = type,
    subtype = subtype,
    name = name,
    subname = subname,
    description = description,
    number = number,
    formula = formula,
    outcome = outcome,
    exposure = exposure,
    mediator = mediator,
    strata = strata,
    level = level,
    terms = terms,
    model_info = model_info,
    parameter_estimates = parameter_estimates,
    run = run
  )

  # Return tibble
  tbl
}

#' Model forge initialization
#' @keywords internal
#' @noRd
new_forge <- function(x = tibble(),
                      data_list = data_list()) {

  # Validation
  stopifnot(is.data.frame(x))
  vec_assert(data_list, ptype = data_list())

  tibble::new_tibble(
    x,
    data_list = data_list,
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

  new_forge(out, data_list = dl)
}

#' @export
forge_cast <- function(x, to, ..., x_arg = "", to_arg = "") {
  out <- tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)

  # Combine new data
  dl <- c(attributes(x)$data_list, attributes(to)$data_list)

  new_forge(out, data_list = dl)
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
  tib_cast(x, to, ...)
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
  df_cast(x, to, ...)
}

#' @export
vec_cast.data.frame.forge <- function(x, to, ...) {
  df_cast(x, to, ...)
}

### Model Archetypes
