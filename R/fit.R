#' @importFrom generics fit
#' @export
generics::fit

#' @export
fit.formula_archetype <- function(object,
                                  .fit,
                                  ...,
                                  data,
																	name = deparse1(substitute(object)),
																	archetype = FALSE) {
  cl <- match.call()
  args <- list(...)

  # Validate functions
  if (!is.function(eval(cl[[3]]))) {
    stop(
      "The `.fit = ",
      paste(cl[[3]]),
      "` is not yet an accepted function for model fitting."
    )
  }
  .fn <- as.character(cl[[3]])

  # Manage data
  stopifnot(is.data.frame(data))

  # Create models and tags for the models
  ml <- list()
  nms <- character()

  for (i in seq_along(object)) {

  	f <- stats::formula(field(object[i], "formula"))
  	strata <-
  		vec_data(object[i])$strata[[1]] |>
  		as.character()

  	if (length(strata) == 0) {
  		args$data <- quote(data)
	    m <- do.call(.fn, args = c(formula = f, args))
	    ml <- append(ml, list(m))
	    nms <- append(nms, paste0(name, "_", i, "_STRATA_0"))
  	} else {
  		strata_lvls <- unique(data[[strata]])
  		for (j in seq_along(strata_lvls)) {
  			.data <- data[data[[strata]] == strata_lvls[j], ]
	  		args$data <- quote(.data)
		    m <- do.call(.fn, args = c(formula = f, args))
		    ml <- append(ml, list(m))
		    nms <- append(nms, paste0(name, "_FIT_", i, "_STRATA_", j))
  		}
  	}
  }

  # Return names to list of models
  names(ml) <- nms

  # Return
  if (archetype) {
  	model_archetype(ml, fmls = object)
  } else {
  	ml
  }
}

#' @importFrom generics tidy
#' @export
generics::tidy

#' @export
tidy.model_archetype <- function(x,
                                 conf.int = TRUE,
                                 conf.level = 0.95,
                                 exponentiate = TRUE,
                                 ...) {

	# Get lists of models
	lom <- vec_data(x)$model

  purrr::map(
    lom,
    ~ possible_tidy(
      .x,
      conf.int = conf.int,
      conf.level = conf.level,
      exponentiate = exponentiate
    )
  )
}

#' Create a "fail-safe" of tidying fits
#' @noRd
my_tidy <- function(x,
										conf.int = TRUE,
										conf.level = 0.95,
										exponentiate = TRUE,
										...) {
	broom::tidy(x,
							conf.int = conf.int,
							conf.level = conf.level,
							exponentiate = exponentiate)
}

#' Local load of it if not when package starts
#' @noRd
possible_tidy <- purrr::possibly(my_tidy, otherwise = NA, quiet = FALSE)

#' @importFrom generics glance
#' @export
generics::glance

#' @export
glance.model_archetype <- function(x, ...) {
	lom <- vec_data(x)$model
  purrr::map(lom, ~ possible_glance(.x,))
}

#' Create a "fail-safe" of glance at fits
#' @noRd
my_glance <- function(x, ...) {
	broom::glance(x)
}

#' Local load of it if not when package starts
#' @noRd
possible_glance <- purrr::possibly(my_glance, otherwise = NA, quiet = FALSE)
