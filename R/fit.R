#' @importFrom generics fit
#' @export
generics::fit

#' @export
fit.formula_archetype <- function(object,
                                  fitting_function,
                                  ...,
                                  data,
																	tag = character(),
																	archetype = FALSE) {
  cl <- match.call()
  args <- list(...)

  # Validate functions
  if (!is.function(eval(cl[[3]]))) {
    stop(
      "The `fitting_function = ",
      paste(cl[[3]]),
      "` is not yet an accepted function for model fitting."
    )
  }
  .fn <- as.character(cl[[3]])

  # Manage data
  validate_class(data, c("tbl_df", "data.frame"))

  # Create models and tags for the models
  ml <- list()
  nms <- character()
  if (length(tag) == 0) {
  	tag = deparse1(substitute(object))
  }

  for (i in seq_along(object)) {

  	f <- formula(object[i])
  	strata <- vec_data(object[i])$strata

  	if (is.na(strata)) {
  		args$data <- quote(data)
	    m <- do.call(.fn, args = c(formula = f, args))
	    ml <- append(ml, list(m))
	    nms <- append(nms, paste0("S", 0))

  	} else {
  		strata_lvls <- unique(data[[strata]])
  		for (i in seq_along(strata_lvls)) {
  			.data <- data[data[[strata]] == strata_lvls[i], ]
	  		args$data <- quote(.data)
		    m <- do.call(.fn, args = c(formula = f, args))
		    ml <- append(ml, list(m))
		    nms <- append(nms, paste0("S", i))
  		}
  	}

  }

  # On names
  names(ml) <- paste0(tag, "_N", 1:length(ml), "_", nms)

  # Return
  if (archetype) {
  	model_archetype(ml)
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
