#' @importFrom generics fit
#' @export
generics::fit

#' @export
fit.list_of_formulas <- function(object,
								 fitting_function,
								 ...,
								 data,
								 strata = NULL) {

	cl <- match.call()
	args <- list(...)
	validate_class(data, c("tbl_df", "data.frame"))
	args$data <- quote(data)

	if (!is.function(eval(cl[[3]]))) {
		stop("The `fitting_function = ",
			 paste(cl[[3]]),
			 "` is not yet an accepted function for model fitting.")
	}

	.fn <- as.character(cl[[3]])

	y <- lapply(object, function(.x) {
		f <- .x
		do.call(.fn, args = c(formula = f, args))
	})

	y

}