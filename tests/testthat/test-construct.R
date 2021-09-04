test_that("test that levels and strata can be safely constructed", {

	h = mpg ~ X(wt) + cyl + vs
	test = linear_reg() %>% set_engine("lm")

	h1 <-
		hypothesize(
			h,
			combination = "sequential",
			test = test,
			data = mtcars
		)

	study <-
		create_study() %>%
		add_hypothesis(h1) %>%
		construct_map()

	expect_type(study$model_map$fit, "list")

	h2 <- update_hypothesis(h1, combination = "parallel")
	study <-
		study %>%
		add_hypothesis(h2)

	expect_type(study$model_map$tidy, "list")

})
