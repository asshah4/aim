test_that("formula lists can be fit internally to create model lists", {

	# Using list of formulas
	f <- mpg ~ X(wt) + hp + cyl
	object <-
		f |>
		forks::frx(pattern = "sequential") |>
		forks::fmls()

	# Fitting
	fit.list_of_formulas(object = object, .f = lm, data = mtcars)



})
