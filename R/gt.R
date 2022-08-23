#' Subgroup Forest Plot
#'
#' @param x Class `forge` object with models that have been fit
#'
#' @param formula Identifies the relationships of interest, with LHS
#'   representing the outcome, and RHS representing the exposure.
#'
#' @param columns Additional columns that help to describe the subgroup models.
#'   At least one column should be selected from this list. The sequence listed
#'   will reflect the sequence shown in the table. The current options are:
#'
#'   * beta = point estimate value, such as odds ratio or hazard ratio
#'
#'   * conf = inclusion of the confidence interval (presumed to be 95%-ile)
#'
#'   * n = number of observations in each model group
#'
#'   For example: `list(beta ~ "Hazard", conf ~ "95% CI" n ~ "No.")"`
#'
#' @param flip Determine if the odds or hazard ratio should be shown as the
#'   reciprocal values. Instead of a decreasing hazard for every unit increase,
#'   it describes an increasing hazard for every unit decrease.
#'
#' @param axis Argument to help modify the forest plot itself. This is a
#'   list-formula of the following parameters. If they are not named, the
#'   function will attempt to "guess" the optimal parameters. The options are:
#'
#'   * title = label or title for the column describing the forest plot
#'
#'   * lim = x-axis limits
#'
#'   * breaks = x-axis tick marks or break points that should be numbers
#'
#'   * int = x-axis intercept
#'
#'   * lab = label for the x-axis
#'
#'   * scale = defaults to continuous, but may also use a log transformation as
#'   well `c("continuous", "log")`
#'
#'   For example: `list(title ~ "Decreasing Hazard", lab ~ "HR (95% CI))`
#'
#' @param width Describes the width of each column in a list of two-sided
#'   formulas. The RHS is a decimal reflecting the percent each column should
#'   take of the entire table. The forest plot is usually given 30% of the
#'   width.
#'
#' For example: `list(n ~ .1, forest ~ 0.3)`
#'
#' @import ggplot2
#' @export
tbl_forest <- function(object, ...) {
	UseMethod("tbl_forest", object = object)
}

#' @export
tbl_forest.forge <- function(object,
														 formula = formula(),
														 groups = character(),
														 columns = list(beta ~ "Estimate",
														 							 conf ~ "95% CI",
														 							 n ~ "No."),
														 flip = FALSE,
														 axis = list(scale ~ "continuous"),
														 width = list()) {

	# TODO revise how this function works for forge objects versus standard data tables
	# TODO add ability to extract or filter by formulas from forge objects
	# TODO how to decide which LEVEL or number to pick in terms of adjusted models
	# TODO add widths of each column

	# Validate formula
	if (!inherits(formula, "formula")) {
		stop("Object should be of type `formula`, not `", class(formula)[1], "`")
	}
	y <- lhs(formula)
	x <- rhs(formula)

	# Rename selecting columns (for both parameter estimates and model info)
	cols <- formula_to_named_list(columns)
	est_vars <- character()
	mod_vars <- character()
	if ("beta" %in% names(cols)) {
		est_vars <- append(est_vars, "estimate")
	}
	if ("conf" %in% names(cols)) {
		est_vars <- append(est_vars, c("conf.low", "conf.high"))
	}
	if ("n" %in% names(cols)) {
		mod_vars <- append(mod_vars, "nobs")
	}

	# Create basic table
	tbl <-
		object |>
		temper() |>
		dplyr::filter(strata %in% groups) |>
		dplyr::filter(outcome == y & exposure == x) |>
		dplyr::group_by(strata, level) |>
		dplyr::filter(number == max(number)) |>
		dplyr::filter(term == x) |>
		dplyr::ungroup() |>
		dplyr::select(strata, level, term, terms,
									all_of(est_vars), all_of(mod_vars))


	# Reciprocal odds or hazard if needed
	if (flip) {
		tbl <- dplyr::mutate(tbl, across(all_of(est_vars), ~ 1 / .x))

		if ("conf.low" %in% est_vars) {
			tbl <-
				dplyr::rename(tbl, conf.high = conf.low, conf.low = conf.high)
		}

	}

	# Setup plotting variables from the axis argument
	x_vars <- formula_to_named_list(axis)

	if ("lim" %in% names(x_vars)) {
		lim_val <- eval(x_vars$lim)
		xmin <- min(lim_val)
		xmax <- max(lim_val)
	} else {
		xmin <- min(tbl$conf.low, na.rm = TRUE)
		xmax <- min(tbl$conf.high, na.rm = TRUE)
	}

	if ("int" %in% names(x_vars)) {
		xint <- eval(x_vars$int)
	} else {
		xint <- dplyr::case_when(
			xmin < -1 & xmax <= 0 ~ -1,
			xmin > -1 & xmax <= 0 ~ 0,
			xmin < 0 & xmax > 0 ~ 0,
			xmin >= 0 & xmax <= 1 ~ 0,
			xmin >= 0 & xmax > 1 ~ 1
		)
	}

	if ("breaks" %in% names(x_vars)) {
		breaks <- eval(x_vars$breaks)
	} else {
		breaks <- ggplot2::waiver()
	}

	if ("lab" %in% names(x_vars)) {
		lab <- x_vars$lab
	} else {
		lab <- NULL
	}

	if ("scale" %in% names(x_vars)) {
		scale <- x_vars$scale
	} else {
		scale <- "continuous"
	}

	# Make appropriate plots
	plots <-
		tbl |>
		dplyr::group_by(strata, level) |>
		tidyr::nest() |>
		dplyr::mutate(gg = purrr::map(data, ~ {
			ggplot(.x, aes(x = estimate, y = 0)) +
				geom_point(size = 50) +
				geom_linerange(aes(xmax = conf.high, xmin = conf.low, size = 5)) +
				geom_vline(xintercept = xint, linetype = 3, size = 5) +
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
				{
					if(scale == "log") {
						scale_x_continuous(
							trans = "pseudo_log",
							breaks = breaks,
							limits = c(xmin, xmax),
							oob = scales::oob_squish
						)
					} else {
						scale_x_continuous(
							breaks = breaks,
							limits = c(xmin, xmax),
							oob = scales::oob_squish
						)
					}
				}
		})) |>
		tidyr::unnest(data) |>
		dplyr::ungroup() |>
		dplyr::add_row()

	tmp <- plots$gg[[1]]
	tmp$layers[1:2] <- NULL
	btm_axis <-
		tmp +
		xlab(lab) +
		theme(
			axis.text.x = element_text(size = 100, margin = margin(10, 0 , 0 , 0)),
			axis.ticks.x = element_line(size = 5),
			axis.ticks.length.x = unit(30, "pt"),
			axis.title.x = element_text(size = 150, margin = margin(10, 0, 0 , 0)),
			axis.line.x = element_line(
				size = 5,
				arrow = grid::arrow(
					length = grid::unit(50, "pt"),
					ends = "both",
					type = "closed"
				)
			)
		)

	plots$gg[nrow(plots)] <- list(btm_axis)

	# Create table
	tbl |>
		dplyr::rowwise() |>
		dplyr::mutate(strata = dplyr::case_when(
			strata %in% names(labels(terms)) ~ vec_data(get_runes(terms, field = "runes", value = strata))$label,
			!(strata %in% names(labels(terms))) ~ strata
		)) |>
		dplyr::ungroup() |>
		dplyr::mutate(ggplots = NA) |>
		dplyr::add_row() |>
		dplyr::select(level, strata, all_of(mod_vars), all_of(est_vars), ggplots) |>
		gt::gt(rowname_col = "level", groupname_col = "strata") |>
		gt::cols_merge(columns = all_of(est_vars),
									 pattern = "{1} ({2}, {3})") |>
		gt::fmt_number(
			columns = where(is.numeric),
			drop_trailing_zeros = TRUE,
			n_sigfig = 2
		) |>
		gt::text_transform(
			locations = gt::cells_body(columns = ggplots),
			fn = function(x) {
				purrr::map(plots$gg,
									 gt::ggplot_image,
									 height = gt::px(50),
									 aspect_ratio = 5)
			}
		) |>
		gt::cols_width(ggplots ~ gt::pct(50)) |>
		gt::cols_width(estimate ~ gt::pct(40)) |>
		gt::cols_width(nobs ~ gt::pct(10)) |>
		gt::opt_vertical_padding(scale = 0) |>
		gt::opt_table_outline(style = "none") |>
		gt::tab_options(
			data_row.padding = gt::px(0),
			table_body.border.bottom.width = gt::px(0),
			table_body.border.top.width = gt::px(0),
			column_labels.border.top.width = gt::px(0)
		) |>
		gt::tab_style(
			style = list(
				gt::cell_text(color = "white", size = gt::px(0)),
				gt::cell_borders(sides = "all", color = NULL)
			),
			locations = list(
				gt::cells_body(columns = ggplots),
				gt::cells_row_groups(groups = "NA"),
				gt::cells_stub(rows = is.na(level))
			)
		) |>
		gt::tab_style(
			style = list(
				gt::cell_text(color = "white", size = gt::px(0))
			),
			locations = list(
				gt::cells_body(columns = c(all_of(mod_vars), all_of(est_vars)),
											 rows = is.na(level))
			)
		) |>
		gt::tab_style(
			style = list(
				gt::cell_borders(sides = "all", color = NULL)
			),
			locations = list(
				gt::cells_body(columns = c(all_of(mod_vars), all_of(est_vars))),
				gt::cells_stub(rows = gt::everything())
			)
		) |>
		gt::cols_label(
			estimate = cols$beta,
			ggplots = x_vars$title,
			nobs = cols$n
		)

}


#' Compact and minimal theme for `gt` tables
#'
#' This theme was used for placing somewhat larger tables into `xaringan` slides
#' by making the spacing more compact and decreasing the font size. The exposed
#' variables are to control font size and table width, but any option from the
#' `gt` package is allowed.
#'
#' @inheritParams gt::tab_options
#' @param ... For passing additional arguments to the [gt::tab_options()]
#'   function
#' @family visualizers
#' @export
theme_gt_compact <- function(data,
														 table.font.size = gt::pct(80),
														 table.width = gt::pct(90),
														 ...) {

	validate_class(data, "gt_tbl")

	data %>%
		gt::tab_options(
			# Preset
			table.margin.left = gt::px(1),
			table.margin.right = gt::px(1),
			row_group.padding = gt::px(1),
			data_row.padding = gt::px(1),
			footnotes.padding = gt::px(1),
			source_notes.padding = gt::px(1),
			stub.border.width = gt::px(1),
			# User supplied
			table.width = table.width,
			table.font.size = table.font.size,
			...
		)

}
