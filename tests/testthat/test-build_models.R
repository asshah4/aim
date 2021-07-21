test_that("correct output of the built models", {
	library(parsnip)

	proj <-
		project() %>%
		set_data(mtcars) %>%
		add_hypothesis(
			name = "weight",
			formula = wt ~ vs,
			test = "t.test",
			paired = TRUE,
			combination = "direct"
		) %>%
		add_hypothesis(
			name = "mpg",
			formula = mpg ~ wt + vs,
			combination = "sequential",
			test = linear_reg() %>% set_engine("lm")
		) %>%
		build_models()

	expect_type(proj$findings, "list")
	expect_length(proj$findings, 2)
	expect_true("tidied" %in% names(proj$findings$weight))

	res <- unlist(proj$findings, recursive = FALSE)
	expect_length(res, 10)

})
