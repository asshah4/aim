test_that("`mdl` objects can be retrieved from fit", {

	data("mtcars")

	# Explicit
	expect_s3_class(mdl(), "mdl")
	x <- lm(mpg ~ wt + hp, data = mtcars)
	m <- mdl(
		x,
		formulas = fmls(mpg ~ wt + hp),
		data_name = "mtcars",
		strata_info = list()
	)
	expect_s3_class(m, "mdl")

	# Simple
	object <- fmls(mpg ~ wt + hp)
	m1 <- fit(object, .fit = lm, data = mtcars, raw = FALSE)
	expect_s3_class(m1, "mdl")

	# Stratified
	object <- fmls(mpg ~ wt + hp + .s(am))
	m2 <- fit(object, .fit = lm, data = mtcars, raw = FALSE)
	expect_s3_class(m2, "mdl")
	expect_length(m2, 2)


})
