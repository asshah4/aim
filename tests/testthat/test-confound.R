test_that("confounders can be found and manipulated", {
	library(parsnip)
	h = mpg + wt ~ X(hp) + X(qsec) + disp + drat + cyl
	test = linear_reg() %>% set_engine("lm")

	h1 <-
		hypothesize(
			h,
			combination = "sequential",
			test = test,
			data = mtcars,
		)

	x <-
		create_study() %>%
		add_hypothesis(h1) %>%
		construct_map() %>%
		reconstruct("h1") %>%
		extract_models("h1_cut")

	expect_length(x, 12)

})
