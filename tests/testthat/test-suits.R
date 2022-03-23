test_that("a formula list can be fit internally to create a model suit", {

	# Using list of formulas
	x <-
		rx(mpg ~ X(wt) + hp + cyl,
			 label = list(wt ~ "Weight", mpg ~ "Mileage")) |>
		fmls(tag = "temp", pattern = "sequential")

	# Fitting
	y <- fit(object = x, fitting_function = lm, data = mtcars, name = "test")
	expect_type(y, "list")
	expect_length(y, 3)
	expect_named(y, paste0("test", sep = "_", 1:3))

	# Create model suit
	out <- model_suit(x, fitting_function = lm, data = mtcars)

})


test_that("several independent models can be added together in a list/suit", {

	m1 <- lm(mpg ~ hp + cyl, mtcars)
	m2 <- lm(mpg ~ hp + gear, mtcars)
	m3 <- glm(am ~ hp + cyl, mtcars, family = "binomial")
	m4 <- glm(am ~ hp + gear, mtcars, family = "binomial")
	m5 <- lm(hp ~ wt, mtcars)

})

