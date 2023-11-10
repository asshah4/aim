test_that("generation of forest plots", {

	object <-
		fmls(mpg ~ hp + drat + wt + .s(am),
				 pattern = 'parallel') |>
		fit(
			.fn = lm,
			data = mtcars,
			raw = FALSE
		) |>
		mdl_tbl()

	outcomes <- mpg ~ 'Miles per gallon'

	terms <- list(
		hp ~ 'Horsepower',
		drat ~ 'Drag',
		wt ~ 'Weight'
	)




})
