test_that('interaction estimates can be made', {
	# Survival model
	library(survival)
	dat <- na.omit(lung)

	expect_message(
		x <-
			fmls(Surv(time, status) ~ .x(age) + .i(sex),
					 pattern = 'direct') |>
			fit(.fn = coxph, data = dat, raw = FALSE)
	)

	object <- model_table(int_sex = x)
	expect_s3_class(object, 'mdl_tbl')
	expect_equal(nrow(object), 1)

	# Since sex is a two level structure, interaction must happen at both levels
	



})
