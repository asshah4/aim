test_that("generate forest plot for strata terms along all variables", {

	object <-
		fmls(vs ~ mpg + disp + wt + .s(am),
				 pattern = 'parallel') |>
		fit(
			.fn = glm,
			family = 'binomial',
			data = mtcars,
			raw = FALSE
		) |>
		mdl_tbl()

	outcomes <- vs ~ 'Vital signs'

	terms <- list(
		mpg ~ 'Mileage',
		disp ~ 'Display',
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
		list(
			title ~ 'Increasing Estimate',
			lab ~ 'Estimate (95% CI)',
			lim ~ c(0, 5)
		)

	## TODO
	width <- list{
	}

	# Simple binomial
	x <- tbl_stratified_forest(
		object = object,
		data = mtcars,
		outcomes = outcomes,
		terms = terms,
		strata = strata,
		level = level,
		columns = columns,
		axis = axis
	)

	expect_s3_class(x, 'gt_tbl')

})
