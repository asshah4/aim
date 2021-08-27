test_that("models can be extracted if available", {
	library(parsnip)
	h = mpg ~ X(wt) + hp + disp
	combination = "sequential"
	test = linear_reg() %>% set_engine("lm")

	h1 <-
		hypothesize(
			h,
			combination = "parallel",
			test = test,
			data = mtcars,
		)
	h2 <- update_hypothesis(h1, combination = "sequential")

	x <-
		study() %>%
		draw(h1)

	# Extract unfitted should error
	expect_error({
		extract_models(x)
	})

	# Fit some models
	y <-
		x %>%
		construct() %>%
		draw(h2)

	# Extract tidy models
	m <- extract(y)
	expect_length(m, 11)
	expect_equal(nrow(m), 6)

	# Extract raw models
	m <- extract(y, tidy = FALSE)
	expect_named(m, expected = c("name", "outcomes", "exposures", "number", "fit"))

	# Message on pulling unfitted model by name
	expect_message({
		extract(y, which_ones = "h2")
	})

})
