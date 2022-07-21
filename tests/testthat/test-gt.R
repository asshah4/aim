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
	f <- fmls(x, order = 2)
	fits <- fit(f, .fit = lm, data = test_data, archetype = TRUE)
	m <- mdls(fits)

	mod <-
		m |>
		dplyr::select(name, strata, level, terms, parameter_estimates, model_info) |>
		dplyr::filter(strata == "vs")

	est <-
		mod$parameter_estimates |>
		dplyr::bind_rows(.id = "level") |>
		dplyr::filter(term == "wt") |>
		dplyr::select(-c(std.error, statistic))

	inf <-
		mod$model_info |>
		dplyr::bind_rows(.id = "level") |>
		dplyr::select(level, nobs)


	tbl <-
		dplyr::full_join(est, inf, by = "level") |>
		dplyr::mutate(level = mod$level) |>
		dplyr::mutate(strata = mod$strata)


	# TODO Table of forest plots
	tar_load(subgroup_models)

	tbl <-
		subgroup_models |>
		filter(outcomes == "Surv(death_timeto,death_cv_yn)") |>
		filter(str_detect(term, "hf_stress_rest_delta_zn")) |>
		filter(number == 9) |>
		select(name, level, estimate, conf.low, conf.high) |>
		mutate(across(c(estimate, conf.low, conf.high), ~ 1 / .x)) |>
		rename(conf.low = conf.high,
					 conf.high = conf.low)



	plots <-
		tbl |>
		add_row() |>
		group_by(name, level) |>
		nest() |>
		mutate(gg = map(data,
										~ ggplot(.x, aes(x = estimate, y = 0)) +
											geom_point(size = 50) +
											geom_linerange(aes(xmax = conf.high,
																				 xmin = conf.low),
																		 size = 5) +
											geom_vline(xintercept = 1, linetype = 3, size = 5) +
											theme_minimal() +
											theme(
												axis.text.y = element_blank(),
												axis.title.y = element_blank(),
												axis.text.x = element_blank(),
												axis.title.x = element_blank(),
												axis.line.x = element_blank(),
												legend.position = "none",
												panel.grid.major = element_blank(),
												panel.grid.minor = element_blank()
											) +
											scale_x_continuous(breaks = c(0, 1, 2, 5),
																				 limits = c(-1, 6),
																				 oob = squish) +
											coord_cartesian(xlim = c(-1, 6), ylim = c(-0.1, 0.1), clip = "off")
		)) |>
		unnest(data) |>
		ungroup()

	x <- plots$gg[[1]]
	x$layers[[1]] <- NULL
	x$layers[[1]] <- NULL

	bottom_axis <-
		x +
		xlab("HR (95% CI)") +
		theme(
			axis.text.x = element_text(size = 100, margin = margin(10, 0 , 0 , 0)),
			axis.ticks.x = element_line(size = 5),
			axis.ticks.length.x = unit(30, "pt"),
			axis.title.x = element_text(size = 150, margin = margin(10, 0, 0 , 0)),
			axis.line.x = element_line(size = 5, arrow = arrow(length = unit(50, "pt"),
																												 ends = "both",
																												 type = "closed"))
		)

	plots$gg[15] <- list(bottom_axis)


	tbl |>
		add_row() |>
		mutate(name = case_when(
			str_detect(name, "age") ~ "Increased Age (> median age)",
			str_detect(name, "cabg") ~ "Prior CABG",
			str_detect(name, "lvef") ~ "LVEF < 50%",
			str_detect(name, "msimi") ~ "Mental Stress Induced Myocardial Ischemia",
			str_detect(name, "psimi") ~ "Conventional Stress Induced Myocardial Ischemia",
			str_detect(name, "race") ~ "African American Race",
			str_detect(name, "sex") ~ "Female Sex"
		)) |>
		mutate(ggplots = NA) |>
		gt(rowname_col = "level", groupname_col = "name") |>
		cols_merge(columns = c(estimate, conf.low, conf.high),
							 pattern = "{1} ({2}, {3})") |>
		fmt_number(
			columns = where(is.numeric),
			drop_trailing_zeros = TRUE,
			n_sigfig = 2
		) |>
		cols_label(estimate = "Hazard Ratio (95% CI)",
							 ggplots = "Increasing Mortality for Low HF HRV") |>
		text_transform(locations = cells_body(columns = ggplots),
									 fn = function(x) {
									 	map(plots$gg, ggplot_image, height = px(50), aspect_ratio = 5)
									 }) |>
		cols_width(ggplots ~ px(300)) |>
		cols_width(estimate ~ px(300)) |>
		opt_vertical_padding(scale = 0) |>
		opt_table_outline(style = "none") |>
		tab_options(
			data_row.padding = px(0),
			table_body.border.bottom.width = px(0),
			table_body.border.top.width = px(0),
			column_labels.border.top.width = px(0)
		) |>
		tab_style(style = list(cell_text(color = "white", size = px(0)),
													 cell_borders(sides = "all", color = NULL)),
							locations = list(cells_body(columns = ggplots),
															 cells_row_groups(groups = "NA"),
															 cells_stub(rows = is.na(level)))) |>
		tab_style(style = list(cell_text(color = "white", size = px(0)),
													 cell_borders(sides = "all", color = NULL)),
							locations = list(cells_body(columns = estimate,
																					rows = is.na(level))))


})


