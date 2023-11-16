test_that("generate forest plot for strata terms along multiple variables", {

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
	terms <-
		list(mpg ~ 'Mileage',
				 disp ~ 'Display',
				 wt ~ 'Weight')
	strata <-
		list(am ~ 'Automatic Transmission')
	level <-
		list(am ~ c('Manual', 'Automatic'))
	columns <-
		list(beta ~ "Estimate",
				 conf ~ "95% CI",
				 n ~ "No.")
	axis <-
		list(title ~ 'Increasing Estimate',
				 lab ~ 'Estimate (95% CI)',
				 lim ~ c(0, 5))
	width <-
		list(n ~ .1,
				 beta ~ .4,
				 forest ~ .5)

	forest <-
		list(size ~ 1,
				 shape ~ 'circle',
				 linetype ~ 3,
				 linewidth ~ 1)

	# Simple binomial
	x <- tbl_stratified_forest(
		object = object,
		data = mtcars,
		outcomes = outcomes,
		terms = terms,
		strata = strata,
		level = level,
		columns = columns,
		axis = axis,
		width = width,
		forest = forest
	)

	expect_s3_class(x, 'gt_tbl')

})

test_that('standard forest plot for 1 term and multiple subgroups can be made', {

	# Subgroups for survival
	library(survival)
	object <-
		fmls(Surv(time, status) ~ age + ph.karno + .s(sex)) |>
		fit(coxph, data = lung, raw = FALSE) |>
		model_table()

	outcomes <- Surv(time, status) ~ 'All-cause mortality'
	terms <-
		list(age ~ 'Age')
	strata <-
		list(sex ~ 'Sex')
	level <-
		list(1 ~ 'Male',
				 2 ~ 'Female')
	columns <-
		list(n ~ 'No.',
				 beta ~ 'Hazard',
				 conf ~ '95% CI')
	width <-
		list(n ~ .1,
				 beta ~ .4,
				 forest ~ .5)
	axis <-
		list(title ~ 'Increasing Hazard',
				 int ~ 1,
				 lab ~ 'HR (95% CI)',
				 scale ~ 'log')
	forest <- list()

	x <- tbl_stratified_forest(
		object = object,
		data = survival::lung,
		outcomes = outcomes,
		terms = terms,
		strata = strata,
		level = level,
		columns = columns,
		axis = axis,
		width = width,
		forest = forest
	)

})
