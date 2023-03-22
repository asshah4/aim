test_that("model tables can be created", {

	# Planning
	x <- lm(mpg ~ wt, mtcars)
	f <- fmls(mpg ~ wt)
	mi <- glance(x)
	pe <- tidy(x, conf.int = TRUE)
	y <- plan_model_table(
		model = "lm",
		formulas = f,
		data_info = list(strata = NA),
		model_fit = mi,
		parameter_estimates = pe,
		run = TRUE
	)
	expect_s3_class(y, "tbl_df")

	# Empty builds
	x <- build_model_table(plan_model_table())
	expect_length(x, 6)
	expect_equal(md_tbl(), x)

})
