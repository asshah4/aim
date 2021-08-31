test_that("confounders can be found and manipulated", {
	library(parsnip)
	h = mpg + wt ~ X(hp) + X(qsec) + disp + drat + cyl
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
		propose(h1) %>%
		propose(h2) %>%
		construct() %>%
		reconstruct("h2") %>%
		reconstruct("h1") %>%
		extract("h2_cut")

	expect_length(x, 11)

})

