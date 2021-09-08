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
		construct_map()

	var_before <- attributes(x)$var_table
	hlist <- find_confounders(x, "h1")
	var_after <- attributes(x)$var_table
	expect_equal(var_before, var_after)

	y <- reconstruct(x, "h1")
	var_after <- attributes(y)$var_table
	expect_length(y$model_map, 8)
	expect_gt(nrow(var_after), nrow(var_before))
	expect_type(var_after$confounders[[1]], "character")

})
