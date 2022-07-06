test_that("lists can be fit", {

	f <- mpg + wt ~ X(hp) + X(cyl) + gear + drat + qsec
	labels <- list(mpg ~ "Mileage", hp ~  "Horsepower")
	tiers <- list(c(drat, qsec) ~ "speed", wt ~ "hardware")
	t <- tm(f, label = labels, tier = tiers)
	x <- rx(t, pattern = "sequential")
	lof <- fmls(x, order = 2)
	expect_s3_class(lof, "formula_archetype")

	lom <- fit(lof, lm, data = mtcars)
	expect_type(lom, "list")
	expect_length(lom, 12)
	expect_s3_class(lom[[1]], "lm")

	# Transform to model archetypes
	m1 <- model_archetype(lom)
	expect_s3_class(m, "model_archetype")
	m2 <- fit(lof, lm, data = mtcars, archetype = TRUE)
	expect_equal(length(m1), length(m2))

	# Tidy
	tbl <- tidy(m)
	expect_type(tbl, "list")
	expect_s3_class(tbl[[1]], "tbl_df")
	expect_length(tbl, 12)

	# Glance
	g <- glance(m)
	expect_type(g, "list")
	expect_s3_class(g[[1]], "tbl_df")
	expect_length(g, 12)


	# Errors


})

test_that("fitting can be done with strata", {

	f <- mpg ~ X(wt) + hp + qsec + S(cyl)
	labels <- list(mpg ~ "Mileage", hp ~  "Horsepower")
	tiers <- list(c(drat, qsec) ~ "speed", wt ~ "hardware")
	t <- tm(f, label = labels, tier = tiers)
	x <- rx(t, pattern = "sequential")
	lof <- fmls(x)
	expect_s3_class(lof, "formula_archetype")

	# Transform to model archetypes
	lom <- fit(lof, lm, data = mtcars)
	ml <- fit(lof, lm, data = mtcars, archetype = TRUE)
	expect_type(lom, "list")
	expect_length(lom, 9)
	expect_s3_class(lom[[1]], "lm")
	m <- model_archetype(lom)
	expect_s3_class(m, "model_archetype")
	expect_equal(ml, m, ignore_attr = TRUE)

	# Tidy
	tbl <- tidy(m)
	expect_type(tbl, "list")
	expect_s3_class(tbl[[1]], "tbl_df")
	expect_length(tbl, 9)

	# Glance
	g <- glance(m)
	expect_type(g, "list")
	expect_s3_class(g[[1]], "tbl_df")
	expect_length(g, 9)

	# Errors


})
