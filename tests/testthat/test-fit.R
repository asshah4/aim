test_that("lists can be fit", {

	f <- mpg + wt ~ X(hp) + X(cyl) + gear + drat + qsec
	labels <- list(mpg ~ "Mileage", hp ~  "Horsepower")
	groups <- list(c(drat, qsec) ~ "speed", wt ~ "hardware")
	t <- term(f, labels = labels, groups = groups)
	x <- rx(t, pattern = "sequential")
	lof <- list_of_formulas(x)
	lom <- fit.list_of_formulas(lof, lm, data = mtcars)
	expect_type(lom, "list")
	expect_length(lom, 12)


})
