test_that("model archetypes can be forged into a table can be initialized", {

	# Multiple objects as a vector
	m1 <- lm(mpg ~ hp + cyl, mtcars)
	m2 <- glm(am ~ hp + cyl, mtcars, family = "binomial")
	m3 <- lm(mpg ~ wt + gear, mtcars)
	ml <- list(first = m1, second = m2, m3)
	x <- md(ml)

	mf <- forge(x)
	expect_equal(nrow(mf), 3)
	expect_length(mf, 14) # Number of columns

	# Basic output
	expect_output(print(mf), "<forge>")
	expect_s3_class(mf, "forge")

})

test_that("unfitted formula archetypes can be forged into a table", {

	s <- rx(mpg + wt ~ hp + cyl)
	x <- fmls(s, order = 2)
	mf <- mdls(x)
	expect_s3_class(mf, "forge")

})

test_that("conversion works between different table types", {

	# Typical models
	m1 <- lm(mpg ~ hp + cyl, mtcars)
	m2 <- lm(mpg ~ wt + gear, mtcars)
	ml <- list(first = m1, m2)
	m <- md(ml)
	x <- mdls(m)

	# Additional models
	s <- rx(mpg + wt ~ hp + cyl)
	f <- fmls(s, order = 2)
	y <- mdls(f, data = mtcars)

	# Combining...
	z <- vec_rbind(x, y)
	expect_s3_class(z, "forge")
	expect_equal(nrow(z), 4)

})
