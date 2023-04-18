test_that("`mdl` objects can be retrieved from fit", {

	data("mtcars")

	# Explicit
	expect_s3_class(mdl(), "mdl")
	x <- lm(mpg ~ wt + hp, data = mtcars)
	f <- fmls(mpg ~ wt + hp)
	m <- mdl(
		x,
		formulas = fmls(mpg ~ wt + hp),
		dataName = "mtcars",
		strataLevels = list()
	)
	expect_s3_class(m, "mdl")

	# Simple
	object <- fmls(mpg ~ wt + hp)
	m1 <- fit(object, .fn = lm, data = mtcars, .unpack = FALSE)
	expect_s3_class(m1, "mdl")
	expect_equal(object, field(m1, "formulas"))

	# Stratified
	object <- fmls(mpg ~ wt + hp + .s(am))
	m2 <- fit(object, .fn = lm, data = mtcars, .unpack = FALSE)
	expect_s3_class(m2, "mdl")
	expect_length(m2, 2)
	expect_length(field(m2, "formulas"), 2)


})
