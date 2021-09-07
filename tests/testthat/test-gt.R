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

	# Create a `gt` table successfully
	y <-
		tbl_sequential(
			x,
			var_list = list(wt = "Weight", hp = "Horsepower", qsec = "Second"),
			group_var = "level",
			ranks = c(1:3),
			rank_lab = "Model",
			stat_col = "p.value"
		)

	expect_s3_class(y, "gt_tbl")

	# Able to shrink the table
	z <-
		y %>%
		theme_gt_compact(table.font.size = gt::px(12)) %>%
		.[["_options"]] %>%
		.[.$parameter == "table_font_size", ] %>%
		.$value %>%
		.[[1]]

	expect_equal(z, "12px")

})
