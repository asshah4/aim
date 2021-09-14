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
		create_map() %>%
		add_hypothesis(h1) %>%
		construct_models()

	var_before <- attributes(x)$relation_table
	hlist <- find_confounders(x, "h1")

	y <- reconstruct(x, "h1")
	var_after <- attributes(y)$relation_table
	expect_length(y, 8)
	expect_gt(nrow(var_after), nrow(var_before))
	expect_type(var_after$confounder[[1]], "character")

})
