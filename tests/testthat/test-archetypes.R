test_that("decorated models can be made from base regressions", {

	# Add term modifications as needed
	tl <- list(mpg ~ "Mileage", hp ~ "Horsepower")
	tr <- list(hp ~ "exposure")

	m1 <- model_archetype(
		lm(mpg ~ hp + cyl, mtcars),
		term_labels = tl,
		term_roles = tr,
		model_label = "LM Test"
	)
	expect_length(m1, 1)
	expect_output(print(m1), "lm")

	m2 <- model_archetype(
		glm(am ~ hp + cyl, mtcars, family = "binomial"),
		term_labels = tl,
		term_roles = tr,
		model_label = "GLM Test"
	)
	expect_length(m2, 1)
	expect_output(print(m2), "glm")

	# Passing objects to the model cards
	z <- lm(mpg ~ wt, data = mtcars)
	m3 <- model_archetype(z)

	# Multiple objects as a vector
	x <- c(m1, m2, m3)

	# Error for lack of models or incorrect inputs
	expect_error(model_archetype("test"))

	# Basic output
	if (isTRUE(requireNamespace("tibble", quietly = TRUE))) {
		tibble::tibble(m1) |>
			print() |>
			expect_output("<mx>")
	}
})

test_that("model specs can also be used to generate decorations", {

	# Model spec of parsip models
	if (isTRUE(requireNamespace("parsnip", quietly = TRUE))) {

		pm <-
			parsnip::linear_reg() |>
			parsnip::set_engine("lm") |>
			parsnip::fit(mpg ~ ., data = mtcars)

		mp <- model_archetype(
			pm,
			term_labels = list(mpg ~ "Mileage"),
			model_label = "Parsnip"
		)

		expect_length(mp, 1)
		expect_output(print(mp), "model_fit_lm")

	}

})
