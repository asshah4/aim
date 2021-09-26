test_that("models can be extracted if available", {
	library(parsnip)
	f <- mpg ~ wt + hp + disp
	exposures <- "wt"
	combination <- "sequential"
	test <- linear_reg() %>% set_engine("lm")

	h1 <-
		hypothesize(
			f,
			exposures = exposures,
			combination = "parallel",
			test = test,
			data = mtcars,
		)
	h2 <- update_hypothesis(h1, combination = "sequential")

	x <-
		create_models() %>%
		add_hypothesis(h1)

	# extract_models unfitted should error
	expect_error({
		extract_models(x)
	})

	# Fit some models
	y <-
		x %>%
		construct_tests() %>%
		add_hypothesis(h2)

	# extract_models tidy models
	m <- extract_models(y)
	expect_length(m, 12)
	expect_equal(nrow(m), 6)

	# extract_models raw models
	m <- extract_models(y, tidy = FALSE)
	expect_named(m, expected = c("name", "outcomes", "exposures", "level", "number", "fit"))

	# Message on pulling unfitted model by name
	expect_message({
		extract_models(y, which_ones = "h2")
	})

})
