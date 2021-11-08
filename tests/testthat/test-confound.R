test_that("confounders can be found and manipulated", {
	library(parsnip)
	f <- mpg + wt ~ hp + qsec + disp + drat + cyl
	test <- linear_reg() %>% set_engine("lm")

	h1 <-
		hypothesize(
			f,
			exposures = c("hp", "qsec"),
			combination = "sequential",
			test = test,
			data = mtcars,
		)

	x <-
		create_models() %>%
		add_hypothesis(h1) %>%
		construct_tests()

	var_before <- attributes(x)$relation_table
	hlist <- find_confounders(x, "h1")

	y <- reconstruct(x, "h1")
	var_after <- attributes(y)$relation_table
	expect_length(y, 7)
	expect_gt(nrow(var_after), nrow(var_before))
	expect_type(var_after$confounders[[1]], "character")

})
