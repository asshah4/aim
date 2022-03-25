test_that("model tables can be initialized", {

	# Add term modifications as needed
	labs <- list(mpg ~ "Mileage", hp ~ "Horsepower", am ~ "Automatic")
	rls <- list(hp ~ "exposure", mpg ~ "outcome")

	m1 <- model_archetype(
		lm(mpg ~ hp + cyl, mtcars),
		term_labels = labs,
		term_roles = rls,
		model_label = "LM Test"
	)

	m2 <- model_archetype(
		glm(am ~ hp + cyl, mtcars, family = "binomial"),
		model_label = "GLM Test",
		term_labels = labs
	)

	m3 <- model_archetype(lm(mpg ~ wt, data = mtcars), model_label = "Simple Test")

	# Multiple objects as a vector
	x <- c(m1, m2, m3)

})
