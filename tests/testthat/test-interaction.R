test_that('interaction estimates can be made', {
	# Survival model
	library(survival)
	dat <- na.omit(lung)

	expect_message(
		x <-
			fmls(Surv(time, status) ~ .x(age) + .i(sex) + ph.karno,
					 pattern = 'sequential') |>
			fit(.fn = coxph, data = dat, raw = FALSE)
	)

	mt <- model_table(int_sex = x)
	expect_s3_class(mt, 'mdl_tbl')
	expect_equal(nrow(mt), 4)
	object <- dplyr::filter(mt, interaction == 'age:sex')
	expect_equal(nrow(object), 1)

	# Since sex is a two level structure, interaction must happen at both levels

	fmls(out ~ .x(age) + .i(sex) + .i(race) + demo, pattern = 'sequential')

	dat <-
		mtcars |>
		dplyr::mutate(cyl = factor(cyl))

	lm(mpg ~ am*cyl, data = dat) |>
		summary()



})
