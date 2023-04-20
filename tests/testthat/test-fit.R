test_that("`fmls` objects can be fitted", {

	# Reference models
	data("mtcars")
	m0 <- lm(mpg ~ wt + hp, data = mtcars)

	# Original models
	object <- fmls(mpg ~ wt + hp)
	args <- list(model = TRUE)
	data <- mtcars
	m1 <- fit(object, .fn = lm, data = mtcars, raw = TRUE)
	expect_equal(class(m0), class(m1[[1]]))

	# <mdl> subtypes
	m1 <- fit(object, .fn = lm, data = mtcars, raw = FALSE)
	expect_s3_class(m1, "mdl")
	expect_length(m1, 1)


	# Stratified, should have two models
	object <- fmls(mpg ~ wt + hp + .s(am))
	m2 <- fit(object, .fn = lm, data = mtcars, raw = TRUE)
	expect_length(m2, 2)
	expect_s3_class(m2[[1]], "lm")

	# Should also keep strata term information when "tidied" into <mdl> object
	m2 <- fit(object, .fn = lm, data = mtcars, raw = FALSE)
	expect_s3_class(m2, "mdl")
	expect_equal(field(m2, "dataArgs")[[1]]$strataVariable, "am")
	expect_equal(field(m2, "dataArgs")[[1]]$dataName, "mtcars")
	expect_equal(field(m2, "dataArgs")[[1]]$strataLevel, 1)
	expect_equal(field(m2, "dataArgs")[[2]]$strataLevel, 0)


})
