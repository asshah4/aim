test_that("creation of a map is successful", {
	library(parsnip)

	# Basics
	m <- create_map()
	expect_length(attributes(m), 9)
	expect_s3_class(m, "model_map")

	# Adding hypothesis and fitting
	h1 <-
		hypothesize(
			mpg ~ wt + hp + cyl,
			combination = "parallel",
			test = linear_reg() %>% set_engine("lm"),
			data = mtcars,
			strata = "vs"
		)

	m <-
		create_map() %>%
		add_hypothesis(h1) %>%
		construct_models()

	expect_type(m$fit, "list")
	expect_s3_class(m$tidy[[1]], "tbl_df")
	expect_output(print(m), "map|hypothes")

	h2 <- update_hypothesis(h1, combination = "parallel")

	m <-
		m %>%
		add_hypothesis(h2) %>%
		construct_models()

	# Should be a list of formulas with only a single data set saved
	expect_equal(nrow(m), 12)
	expect_equal(nrow(attr(m, "data_list")), 1)
	expect_equal(nrow(attr(m, "data_table")), 2)

	# Unadjusted h1 and h2 should be the same
	t1 <- fetch_tidy(m, "h1")
	t2 <- fetch_tidy(m, "h2")
	expect_identical(t1[[1]], t2[[1]])

	# Print and summary functions
	expect_output(print.model_map(m), "map|hypothes")
	expect_output(summary.model_map(m), "Summary|Hypothesis|Data")

})
