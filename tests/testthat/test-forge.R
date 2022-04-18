test_that("model archetypes can be forged into a table can be initialized", {

	# Multiple objects as a vector
	m1 <- lm(mpg ~ hp + cyl, mtcars)
	m2 <- glm(am ~ hp + cyl, mtcars, family = "binomial")
	m3 <- lm(mpg ~ wt + gear, mtcars)
	x <-
		list(first = m1, second = m2, m3) |>
		model_archetype()

	mf <- model_forge(x)
	expect_equal(nrow(mf), 3)
	expect_length(mf, 14) # Number of columns

	# Basic output
	expect_output(print(mf), "<model_forge>")
	expect_s3_class(mf, "model_forge")

})

test_that("unfitted formula archetypes can be forged into a table", {

	s <- rx(mpg + wt ~ hp + cyl)
	x <- fmls(s, order = 2)


})
