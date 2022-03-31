test_that("lists can be fit", {

	f <- mpg + wt ~ X(hp) + X(cyl) + gear + drat + qsec
	labels <- list(mpg ~ "Mileage", hp ~  "Horsepower")
	tiers <- list(c(drat, qsec) ~ "speed", wt ~ "hardware")
	t <- tm(f, label = labels, tier = tiers)
	x <- rx(t, pattern = "sequential")
	lof <- construct_script(x)
	expect_s3_class(lof, "formula_archetype")

	lom <- fit(lof, lm, data = mtcars)
	expect_type(lom, "list")
	expect_length(lom, 12)
	expect_s3_class(lom[[1]], "lm")

	# Errors


})
