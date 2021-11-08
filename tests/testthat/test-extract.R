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

	# extract_results unfitted should error
	expect_error({
		extract_results(x)
	})

	# Fit some models
	y <-
		x %>%
		construct_tests() %>%
		add_hypothesis(h2)

	# Message on pulling unfitted model by name
	expect_message({
		extract_results(y, which_ones = "h2")
	})

	# extract_results tidy models
	m <- extract_results(y, how = "tidy", flat = TRUE, exponentiate = FALSE, conf.level = 0.95, conf.int = TRUE)
	expect_length(m, 12)
	expect_equal(nrow(m), 6)

	# extract_results raw models
	m <- extract_results(y)
	expect_named(m, expected = c("name", "outcomes", "exposures", "level", "number", "formulae", "fit"))

	# Testing glance feature
	df <- mtcars
	df$am <- as.factor(df$am)
	h3 <- hypothesize(
		am ~ mpg + wt,
		combination = "sequential",
		test = logistic_reg() %>% set_engine("glm"),
		data = df
	)

	z <-
		y %>%
		add_hypothesis(h3) %>%
		construct_tests()

	# Extract glance
	m <-
		z %>%
		extract_results(which_ones = "h3", how = "glance") %>%
		extract_results(which_ones = "h3", how = "tidy")

	flatten(m)

})
