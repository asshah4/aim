test_that("sequential gt tables can be made ", {

	f <- mpg ~ wt + hp + qsec + cyl + vs + gear
	ptest <- stats::lm
	exposures <- c("wt", "hp", "qsec")

	h1 <-
		hypothesize(
			f,
			exposures,
			combination = "sequential",
			test = ptest,
			data = mtcars,
			strata = "am"
		)

	x <-
		create_models() %>%
		add_hypothesis(h1) %>%
		construct_tests() %>%
		extract_results(how = "tidy") %>%
		extract_results(how = "glance") %>%
		flatten()

	# Create a `gt` table successfully
	y <-
		tbl_sequential(
			x,
			terms = term ~ list("wt", "hp", "qsec"),
			by = level ~ list("0" = "Manual", "1" = "Automatic"),
			models = number ~ list(1:3),
			model_label = "Model",
			model_notes = list("Mileage", "Exposure", "Cylinders", "Gear"),
			values = c("estimate", "conf.low", "conf.high", "r.squared"),
			pattern = "{1} ({2}, {3}); R^2 = {4}",
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

test_that("comparison gt tables can be made", {

	h1 <- hypothesize(
		x = mpg ~ hp + cyl,
		exposures = "hp",
		test = stats::lm,
		combination = "direct",
		data = mtcars
	)

	h2 <- hypothesize(
		x = mpg ~ hp + am,
		exposures = "hp",
		test = stats::lm,
		combination = "direct",
		data = mtcars
	)

	expect_message({
		x <-
			create_models() %>%
			add_hypothesis(h1) %>%
			add_hypothesis(h2) %>%
			construct_tests() %>%
			extract_results(how = "tidy") %>%
			flatten()
	})

	y <- tbl_compare(
		data = x,
		terms = term ~ list(hp = "Horsepower", cyl = "Cylinders", am = "Transmission"),
		models = name ~ list(h1 = "First", h2 = "Second"),
		statistic = p.value ~ 0.05,
		values = c("estimate", "conf.low", "conf.high"),
		pattern = "{1} ({2}, {3})",
		style = fill ~ list(color = "lightgreen"),
		decimals = 2,
		missing_text = "-"
	)

	expect_s3_class(y, "gt_tbl")

})

test_that("gt tables can easily be made to represent multiple models", {

	# Create models
	hseq <- hypothesize(
		x = mpg + qsec ~ wt + cyl + drat + am,
		exposures = c("wt", "cyl"),
		combination = "sequential",
		test = stats::lm,
		data = mtcars
	)
	hpar <- update_hypothesis(hseq, combination = "parallel")
	maps <-
		create_models() %>%
		add_hypothesis(hseq) %>%
		add_hypothesis(hpar) %>%
		construct_tests() %>%
		extract_results(how = "tidy")

	object <- flatten(maps) %>% subset(., name == "hseq")


	x = "exposures"
	y = outcomes ~ list(mpg = "Mileage", qsec = "Acceleration")
	id = "number"
	terms = term ~ list("wt", "drat", am = "Transmission")
	values = c("estimate", "conf.low", "conf.high")
	pattern = "{1} ({2}, {3})"
	statistic = p.value ~ 0.05
	style = fill ~ list(color = "lightgreen")
	decimals = 2
	missing_text = "-"




})

test_that("subgroup models can be made, with a forest plot", {

	# Outcome = am
	# Exposure = wt (and binary)
	# Adjustment = mpg
	# Subgroup = vs
	test_data <-
		mtcars |>
		dplyr::mutate(heavyweight = dplyr::if_else(wt < median(wt), 0, 1))

	x <- rx(mpg ~ X(wt) + S(vs) + S(am), pattern = "direct")
	f <- fmls(
		x,
		label = list(mpg ~ "Mileage", vs ~ "Vroom Sounds", am ~ "Automatic Transmission"),
		order = 2
	)
	fits <- fit(f, .fit = lm, data = test_data, archetype = TRUE)
	m <- mdls(fits)


	tbl <- tbl_forest(object = m, y = "mpg", x = "wt", groups = c("vs", "am"), levels = c("0", "1"), xlab = "HR (95% CI)", xlim = c(-10, 0), xbreak = c(0, -1, -2, -5, -10))

	expect_s3_class(tbl, "gt_tbl")
	expect_s3_class

})


