test_that("different models can be mapped together", {

	# Complex model list
	f <- mpg ~ hp + gear + cyl
	m1 <- lm(f, data = mtcars)
	m2 <-
		f |>
		forks::frx(pattern = "parallel", labels = list(mpg ~ "Mileage"))
		forks::fmls(name = "list_ex")
		list_of_models(lm, data = mtcars)
	x <- list()
})
