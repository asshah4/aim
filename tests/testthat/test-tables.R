test_that("formula lists can be fit internally to create model lists", {

	# Create list of models
	f <- mpg ~ X(wt) + hp + cyl
	object <-
		f |>
		forks::frx(pattern = "sequential", labels = list(wt ~ "Weight", mpg ~ "Mileage")) |>
		forks::fmls(name = "temp")
	out <- fit(object = object, .f = lm, data = mtcars)
	lom <- list_of_models(object, lm, data = mtcars)
	tbl <- explode.list_of_models(lom)
	expect_length(labels(tbl), 2)


})

