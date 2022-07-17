test_that("multiple inputs will be returned appropriately", {

	# Models
	m1 <- lm(mpg ~ hp + cyl, mtcars)
	m2 <- glm(am ~ hp + cyl, mtcars, family = "binomial")
	m3 <- md(m2)

	# Formulas
	f1 <- mpg ~ wt + cyl
	f2 <- fmls(f1)
	f3 <- fmls(mpg + wt ~ hp + cyl, order = 2)

	# Dot arguments
	args <- list(m1, second = m3, f = f1, f3)
	mc <- as.call(str2lang("list(m1, second = m3, f = f1, f3)"))
	x <- forge(m1, second = m3, f = f1, f3)

	# Names are fixed now
	expect_equal(x$name[1], "m1")
	expect_equal(x$name[2], "second")
	expect_equal(x$name[5], "f3_2")

})

test_that("model archetypes can be forged into a table can be initialized", {

	# Multiple objects as a vector
	m1 <- lm(mpg ~ hp + cyl, mtcars)
	m2 <- glm(am ~ hp + cyl, mtcars, family = "binomial")
	m3 <- lm(mpg ~ wt + gear, mtcars)
	m_named <- list(first = m1, second = m2, third = m3)
	m_mixed <- list(m1, second = m2, third = m3)
	m_none <- list(m1, m2, m3)
	x <- md(m_mixed)

	mf <- forge(x)
	expect_equal(nrow(mf), 3)
	expect_length(mf, 13) # Number of columns

	# Basic output
	expect_output(print(mf), "<forge>")
	expect_s3_class(mf, "forge")

})

test_that("unfitted formula archetypes can be forged into a table", {

	s <- rx(mpg + wt ~ hp + cyl, pattern = "sequential")
	x <- fmls(s, order = 2)
	mf <- mdls(x)
	expect_s3_class(mf, "forge")
	expect_equal(nrow(mf), 4)

})

test_that("conversion works between different table types", {

	# Typical models
	m1 <- lm(mpg ~ hp + cyl, mtcars)
	m2 <- lm(mpg ~ wt + gear, mtcars)
	ml <- list(first = m1, m2)
	m <- md(ml)
	x <- mdls(m)

	# Additional models
	y <-
		rx(mpg + wt ~ hp + cyl) |>
		fmls(order = 2) |>
		mdls(data = mtcars)

	# Combining...
	z <- vec_rbind(x, y)
	expect_s3_class(z, "forge")
	expect_equal(nrow(z), 4)

})

test_that("strata and their levels are noted in the forged table", {

	x <- rx(am ~ X(wt) + mpg + S(vs), pattern = "direct")
	f <- fmls(x, order = 2)
	fits <- fit(f, .fit = glm, family = "binomial", data = test_data, archetype = TRUE)
	m <- mdls(fits)
	expect_true("strata" %in% names(m))


})
