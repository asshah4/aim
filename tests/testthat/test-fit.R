test_that("`fmls` objects can be fitted", {

	data("mtcars")
	m0 <- lm(mpg ~ wt + hp, data = mtcars)

	object <- fmls(mpg ~ wt + hp)
	args <- list(model = TRUE)
	data <- mtcars
	m1 <- fit(object, .fit = lm, data = mtcars)
	expect_equal(class(m0), class(m1[[1]]))

	# Stratified
	object <- fmls(mpg ~ wt + hp + .s(am))
	m2 <- fit(object, .fit = lm, data = mtcars)
	expect_length(m2, 2)

})
