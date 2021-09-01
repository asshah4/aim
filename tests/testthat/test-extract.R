test_that("models can be extract_modelsed if available", {
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
		create_study() %>%
		add_hypothesis(h1)

	# extract_models unfitted should error
	expect_error({
		extract_models(x)
	})

	# Fit some models
	y <-
		x %>%
		construct_map() %>%
		add_hypothesis(h2)

	# extract_models tidy models
	m <- extract_models(y)
	expect_length(m, 11)
	expect_equal(nrow(m), 6)

	# extract_models raw models
	m <- extract_models(y, tidy = FALSE)
	expect_named(m, expected = c("name", "outcomes", "exposures", "number", "fit"))

	# Message on pulling unfitted model by name
	expect_message({
		extract_models(y, which_ones = "h2")
	})

})
