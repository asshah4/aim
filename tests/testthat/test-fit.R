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
	tbl <- tidy(m1)
	expect_type(tbl, "list")
	expect_s3_class(tbl[[1]], "tbl_df")
	expect_length(tbl, 12)

	# Glance
	g <- glance(m1)
	expect_type(g, "list")
	expect_s3_class(g[[1]], "tbl_df")
	expect_length(g, 12)

})

test_that("fitting can be done with strata", {

	f <- mpg ~ X(wt) + hp + qsec + S(cyl) + S(am)
	labels <- list(mpg ~ "Mileage", hp ~  "Horsepower")
	tiers <- list(c(drat, qsec) ~ "speed", wt ~ "hardware")
	t <- tm(f, label = labels, tier = tiers)
	x <- rx(t, pattern = "sequential")
	lof <- fmls(x, order = 2)
	expect_s3_class(lof, "formula_archetype")

	# Transform to model archetypes
	ml <- fit(lof, lm, data = mtcars)
	ma <- fit(lof, lm, data = mtcars, archetype = TRUE)
	expect_type(ml, "list")
	expect_length(ml, 9)
	expect_s3_class(ml[[1]], "lm")
	m <- model_archetype(ml) # Expect to lose information here
	expect_s3_class(m, "model_archetype")
	# Because of how the model lists are made, the strata information will be lost
	# Has to stay within the "archetype" world to maintain that extra information
	# Thus, will not be the same metadata between generic mods and archetype mods
	expect_equal(vec_data(ma)[1], vec_data(m)[1], ignore_attr = TRUE)
	expect_equal(vec_data(ma)[7], vec_data(m)[7], ignore_attr = TRUE)
	expect_equal(vec_data(m)[["strata_info"]][1], NA_character_)

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


})
