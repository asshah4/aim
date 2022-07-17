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

	x <- rx(am ~ X(wt) + mpg + S(vs), pattern = "direct")
	f <- fmls(x, order = 2)
	fits <- fit(f, .fit = glm, family = "binomial", data = test_data, archetype = TRUE)
	m <- mdls(fits)

	tbl <-
		dplyr::bind_rows(m$parameter_estimates, .id = "source") |>
		dplyr::filter(term == "wt") |>
		dplyr::select(term, source, estimate, p.value) |>
		tidyr::pivot_wider(names_from = source, values_from = c("estimate", "p.value"))

	tbl$grp <- "vs"
	tbl$forest <- NA

	tbl |>
		gt(rowname_col = "term", groupname_col = "grp") |>
		cols_merge(columns = ends_with("_1"),
							 pattern = "{1} ({2})") |>
		cols_merge(columns = ends_with("_2"),
							 pattern = "{1} ({2})") |>
		fmt_number(
			columns = starts_with("estimate"),
			drop_trailing_zeros = TRUE,
			n_sigfig = 2
		) |>
		text_transform(
			locations = cells_body(columns = estimate_1),
			fn = function(.x) {
				ggplot(data = tbl, aes(x = estimate_1)) +
					geom_point() |>
					ggplot_image()
			}
		)

})


library(ggplot2)
library(gt)

# make a plot of each mfr
tibble_plot <- gtcars %>%
	group_by(mfr) %>%
	nest() %>%
	mutate(plot = map(data, ~ggplot(., aes(hp, trq, size = msrp)) + #you could use the function and it should work
											geom_point() +
											geom_point(color = "blue") +
											theme(legend.position = "none"))) %>%
	select(-data) %>%
	# Create empty column (a placeholder for the images)
	mutate(ggplot = NA)


# Creates the length of the tibble
text_names <- gtcars %>%
	select(mfr) %>%
	unique() %>%
	pull()


# Is a bit slow for me
tibble_output <- tibble(
	text = text_names,
	ggplot = NA,
	.rows = length(text_names)) %>%
	gt() %>%
	text_transform(
		locations = cells_body(vars(ggplot)),
		fn = function(x) {
			map(tibble_plot$plot, ggplot_image, height = px(200))
		}
	)

tibble_output


library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(gt)

tmp <-
	iris %>%
	group_by(Species) %>%
	nest() %>%
	# calculate a column of global min and max - needed for setting plot limits
	mutate(
		Sepal.Length.Min = min(unlist(map(data, ~.$Sepal.Length))),
		Sepal.Length.Max = max(unlist(map(data, ~.$Sepal.Length))),
		Sepal.Length.Mean = mean(unlist(map(data, ~.$Sepal.Length)))
	) %>%
	# build row plots
	mutate(ggplots = pmap(
		list(data, Sepal.Length.Min, Sepal.Length.Max, Sepal.Length.Mean),
		~ggplot(..1) +
			geom_vline(
				xintercept = ..4,
				size = 2,
				color = "blue") +
			geom_errorbarh(
				mapping = aes(
					xmin = min(..1$Sepal.Length),
					xmax = max(..1$Sepal.Length),
					y = 0),
				size = 2) +
			geom_point(
				mapping = aes(
					x = mean(..1$Sepal.Length),
					y = 0),
				size = 5) +
			scale_x_continuous(limits = c(..2, ..3)) +
			theme_void() +
			theme(
				plot.background = element_blank(),
				panel.background = element_blank())
	)) %>%
	select(Species, ggplots)

tibble(
	groups = tmp$Species,
	ggplots = NA
) |>
	gt() %>%
	text_transform(
		locations = cells_body(vars(ggplots)),
		fn = function(x) {
			map(tmp$ggplots, ggplot_image, height = px(100))
		}
	)
