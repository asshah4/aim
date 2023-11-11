test_that("generate forest plot for strata terms along all variables", {

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

	strata <- list(
		am ~ 'Automatic Transmission'
	)

	level <- list(
		am ~ c('Manual', 'Automatic')
	)

	columns <- list(beta ~ "Estimate",
								 conf ~ "95% CI",
								 n ~ "No.")

	axis <-
		list(title ~ 'Increasing Estimate', lab ~ 'Estimate (95% CI)')


})
