test_that("formula lists can be fit internally to create model lists", {

	# Using list of formulas
	f <- mpg ~ X(wt) + hp + cyl
	object <-
		f |>
		forks::frx(pattern = "sequential", labels = ) |>
		forks::fmls(name = "temp")

	# Fitting
	out <- fit(object = object, .f = lm, data = mtcars)
	expect_type(out, "list")
	expect_length(out, 3)

	# Create list of models
	lom <- list_of_models(object, lm, data = mtcars)
	expect_length(lom, 3)


})
