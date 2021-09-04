test_that("gt tables can be made ", {
	library(parsnip)
	h = mpg ~ X(wt) + X(hp) + X(qsec) + cyl + vs + gear
	test = linear_reg() %>% set_engine("lm")

	h1 <-
		hypothesize(
			h,
			combination = "sequential",
			test = test,
			data = mtcars,
			strata = "am"
		)

	x <-
		create_study() %>%
		add_hypothesis(h1) %>%
		construct_map() %>%
		extract_models("h1")

	gt_tbl <-
		tbl_sequential(
			x,
			var_list = list(wt = "Weight", hp = "Horsepower", qsec = "Second"),
			group_var = "level",
			ranks = c(1:3),
			rank_lab = "Model",
			stat_col = "p.value"
		)

	expect_s3_class(gt_tbl, "gt_tbl")

})
