test_that("model can be initialized", {

	m <- lm(mpg ~ wt + hp, data = mtcars)
})


test_that("model tables can be created", {

	# Empty builds
	x <- md_tbl()
	expect_length(x, 6)
	expect_equal(model_table(), x)

})

test_that("model tables can be initialized with models", {

	# Planning
	object <- fmls(mpg ~ wt + hp + .s(am))
	x <- fit(object, .fn = lm, data = mtcars, .unpack = FALSE)
	m <- field(x, "model")
	mx <- field(x, "model_type")
	fx <- field(x, "formulas")
	dn <- field(x, "data_name")
	si <- field(x, "strata_info")
	dd <- list()
	for (i in 1:length(si)) {
		dd[[i]] <-
			list(
				data_name = dn[i],
				strata_name = names(si[i]),
				strata_level = si[[i]]
			)
	}
	mi <- lapply(m, possible_glance)
	pe <- lapply(m, possible_tidy)
	rn <- rep(TRUE, length(m))
	.plan <- plan_model_table(
		model = mx,
		formulas = fx,
		data_desc = dd,
		model_info = mi,
		parameter_estimates = pe,
		run = rn
	)
	expect_type(.plan, "list")

	# Building
	.build <- build_model_table(.plan)
	expect_s3_class(.build, "tbl_df")

	# Initializing
	.init <- new_model_table(.build)
	expect_s3_class(.init, "md_tbl")

})

