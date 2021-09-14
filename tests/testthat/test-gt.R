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
		create_map() %>%
		add_hypothesis(h1) %>%
		construct_models() %>%
		extract_models("h1")

	# Create a `gt` table successfully
	y <-
		tbl_sequential(
			x,
			terms = term ~ list("wt", "hp", "qsec"),
			by = level ~ list("0" = "Manual", "1" = "Automatic"),
			models = number ~ list(1:3),
			model_label = "Model",
			model_notes = list("Mileage", "Exposure", "Cylinders", "Gear"),
			values = c("estimate", "conf.low", "conf.high"),
			pattern = "{1} ({2}, {3})",
			statistic = p.value ~ 0.05
		)

	expect_s3_class(y, "gt_tbl")
	expect_named(y[["_styles"]]$styles[[1]]$cell_fill, "color")

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
