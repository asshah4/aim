test_that("several models can be added together in a list", {

	m1 <- lm(mpg ~ hp + cyl, mtcars)
	m2 <- lm(mpg ~ hp + gear, mtcars)
	m3 <- glm(am ~ hp + cyl, mtcars, family = "binomial")
	m4 <- glm(am ~ hp + gear, mtcars, family = "binomial")
	m5 <- lm(hp ~ wt, mtcars)

	ml1 <- list(m3, m4)

})

test_that("formula lists can be fit internally to create model lists", {

	# Using list of formulas
	f <- mpg ~ X(wt) + hp + cyl
	object <-
		f |>
		frx(pattern = "sequential",
				labels = list(wt ~ "Weight", mpg ~ "Mileage")) |>
		fmls(name = "temp")

	# Fitting
	out <- fit(object = object, .f = lm, data = mtcars)
	expect_type(out, "list")
	expect_length(out, 3)

	# Create list of models
	lom <- list_of_models(object, lm, data = mtcars)
	expect_length(lom, 3)


})
