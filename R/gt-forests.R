#' Forest plot and table
#'
#' @description
#' Forest plots are usually ways to describe contrasting data, such as between
#' strata, or to show interaction (if present). We can show the estimates of
#' each parameter along a dichotomous subgroup, or we can show the estimates of
#' a primary exposure along a multitude of subgroups. This function allows both
#' methods (and some spectrum in between) to demonstrate these.
#'
#' @inheritParams tbls
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
#'   * p = p_value for model or interaction term
#'
#'   For example: `list(beta ~ "Hazard", conf ~ "95% CI" n ~ "No.")"`
#'
#' @param strata A `<formula>` or list of formulas of strata that should be
#'   evaluated, with the **LHS** referring to the strata and the **RHS**
#'   referring to its label.
#'
#' @param level A `<formula>` or list of formulas where each list-element is a
#'   formula with the **LHS** reflecting either the variable to re-label or a
#'   specific level, and the **RHS** reflecting what the new level should be
#'   called (for display). If there are conflicting labels, the most recent will
#'   be used.
#'
#'   For example, `list(am ~ c("Manual", "Automatic")` would take, from the
#'   `mtcars` dataset, the `am` variable, which consists of `c(0, 1)`, and
#'   relabel them in the order described. They are sorted in ascendiing order
#'   prior to re-labeling.
#'
#'   The alternative approach is to use the specific level itself and have it
#'   re-labeled. `list(0 ~ "Absent")` would take all levels that are zero, and
#'   change their value.
#'
#' @param invert Logical. Determine if the odds or hazard ratio should be shown
#'   as the reciprocal values. Instead of a decreasing hazard for every unit
#'   increase, it describes an increasing hazard for every unit decrease.
#'   Defaults to FALSE
#'
#' @param axis Argument to help modify the forest plot itself. This is a
#'   `<formula>` or list of formulas of the following parameters. If they are not named, the
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
#' @param width Describes the width of each column in a `<formula>` or list of
#'   formulas. The **RHS** is a decimal reflecting the percent each column should
#'   take of the entire table. The forest plot is usually given 30% of the
#'   width. The default options attempt to be sensible. Options, indicated by the term on the **LHS** of the formula, include:
#'
#'   * n = Column describing number of observations
#'
#'   * beta = Column of estimate and confidence intervals (usually combined)
#'
#'   * forest = Column containing forest plots
#'
#' For example: `list(n ~ .1, forest ~ 0.3)`
#'
#' @param forest A `list-formula` object that can be used to help customize the
#'   forest plot prior to generation of the table. The options directly
#'   correspond to `ggplot2` aesthetic specifications that can modify the visual
#'   aspects of the forest plot. The currently supported arguments:
#'
#'   * size = Relative size of the marker for point estimate
#'
#'   * shape = Shape of the marker for point estimate
#'
#'   * fill = Fill of the marker for point estimate
#'
#'   * linetype = Vertical line that serves as the x-intercept across the table
#'
#'   * linewidth = Thickness of lines, for both vertical and horixontal axes
#'
#' @param digits The number of significant figures to present. If the numbers
#'   are not scaled in a presentable fashion, can always adjust the table
#'   subsequently.
#'
#' @import gt ggplot2
#' @name tbl_forest
NULL

#' @rdname tbl_forest
#' @export
tbl_interaction_forest <- function(object,
                                   outcomes = formula(),
                                   exposures = formula(),
                                   interactions = formula(),
                                   exponentiate = FALSE,
                                   ...) {

	# Validation
	# 	Appropriate objects are `mdl_tbl`
	# 	The models must be of the same type for comparison sake
	# 	Must have interaction terms available
	# 	Dataset for interaction must be attached

	# Arguments ----
	checkmate::assert_class(object, 'mdl_tbl')

	## Outcomes

	## Outcomes = outcomes and how to rename
	out <- formulas_to_named_list(outcomes)
	out_nms <- names(out)
	out_lab <- unlist(unname(out))
	checkmate::assert_true(all(out_nms %in% object$outcome))

	## Exposures
	exp <- formulas_to_named_list(exposures)
	exp_nms <- names(exp)
	exp_lab <- unlist(unname(exp))
	checkmate::assert_true(all(exp_nms %in% object$exposure))

	## Interactions
	int <- formulas_to_named_list(interactions)
	int_nms <- names(int)
	int_lab <- unlist(unname(int))
	it <- levels(interaction(exp_nms, int_nms, sep = ":"))
	checkmate::assert_true(all(it %in% object$interaction))

	# Table ----

	## Table setup
	# Each table can only show one outcome ~ exposure relationship
	# Thus, n_tables = n_outcomes x n_exposures
	# Rows are dependent on number of interactions being assessed
	tbl_exp <- list()
	tbl_out <- list()

	# Loop through each outcome and exposure
	for (o in out_nms) {
		for (e in exp_nms) {

			# Limited to the current exposure, outcome, and any interaction terms
			obj <-
				object |>
				dplyr::filter(outcome == o) |>
				dplyr::filter(exposure == e) |>
				dplyr::filter(interaction %in% it)

			# Get interaction terms that are available in this subset
			intVars <-
				obj$interaction |>
				gsub(paste0(e, ":"), "", x = _)

			# Rows of table will be be rows in the <mdl_tbl> object
			# Should be the same length as interaction variables that are available
			n <- 1:nrow(obj)
			stopifnot(
				"Number of interaction variables not equal to number of rows in <mdl_tbl> object." =
					length(intVars) == length(n)
			)

			# List of interaction estimates
			# Need to get p-value from each interaction
			i <- lapply(n, function(.x) {
				estimate_interaction(obj[.x, ], exposure = e, interaction = intVars[.x])
			})


		}
	}


	estimate_interaction(obj[1, ], exposure = exp_nms[1], interaction = int_nms[1])

}

#' @rdname tbl_forest
#' @export
tbl_stratified_forest <- function(object,
																	data,
																	outcomes = formula(),
																	terms = formula(),
																	strata = formula(),
																	level = formula(),
																	columns = list(beta ~ "Estimate",
																								 conf ~ "95% CI",
																								 n ~ "No."),
																	invert = FALSE,
																	axis = list(scale ~ "continuous"),
																	width = list(),
																	forest = list(),
																	digits = 2,
																	...) {


	# Table setup ----

	## Validation
	# 	Ensure correct object type
	# 	Ensure only one model family is present
	# 	Only one outcome for forest plots shoudl be presented, right?
	checkmate::assert_class(object, 'mdl_tbl')
	if (length(unique(object$name)) > 1) {
		stop('Cannot combine models from different datasets or regressions into a table safely.')
	}

	## Outcomes = outcomes and how to rename
	out <- formulas_to_named_list(outcomes)
	out_nms <- names(out)
	out_lab <- unlist(unname(out))
	checkmate::assert_true(all(out_nms %in% object$outcome))

	## Terms = individual parameters to display (and relabel if needed)
	tms <- formulas_to_named_list(terms)
	tms_nms <- names(tms)
	tms_lab <- unlist(unname(tms))

	## Strata = which strata parameters to present
	sta <- formulas_to_named_list(strata)
	sta_nms <- names(sta)
	sta_lab <- unlist(unname(sta))
	checkmate::assert_true(all(sta_nms %in% object$strata))

	## Levels = What are the names for the levels of the strata
	# If multiple strata, may have multiple levels to relabel
	lvl <- formulas_to_named_list(level)
	lvl_nms <- names(lvl)
	lvl_lab <- unlist(unname(lvl))
	if (length(lvl_lab) == 1) {
		lvl_lab <-
			lvl_lab |>
			str2lang()
		as.character() |>
			tail(-1)
	}

	## Columns
	cols <- formulas_to_named_list(columns)
	est_var <- character()
	mod_var <- character()
	if ("beta" %in% names(cols)) {
		est_var <- append(est_var, "estimate")
	}
	if ("conf" %in% names(cols)) {
		est_var <- append(est_var, c("conf_low", "conf_high"))
	}
	if ("p" %in% names(cols)) {
		est_var <- append(est_var, "p_value")
	}
	if ("n" %in% names(cols)) {
		mod_var <- append(mod_var, "nobs")
	}

	## Creating a table
	# Will need to know the number of strata and terms and outcomes
	# Need to know relationships and counts of each to help organize
	# Limit to a single outcome at this time due to simplicity

	stopifnot(
		'`tbl_*_forest()` only displays a single outcome at a time currently. Please file an issue if there is interest in multi-outcome forest tables.' =
			length(out_nms) == 1
	)

	# Create a basic table of required elements
	tbl <-
		object |>
		reduce_models() |>
		# Ensure correct variables are available
		dplyr::filter(strata %in% sta_nms) |>
		dplyr::filter(outcome %in% out_nms) |>
		dplyr::filter(term %in% tms_nms) |>
		# Place in correct columnar order
		dplyr::select(outcome,
									term,
									strata,
									level,
									any_of(mod_var),
									any_of(est_var))


	# Reciprocal odds or hazard if needed
	if (invert) {
		tbl <-
			dplyr::mutate(tbl, across(
				c(all_of(est_var), -any_of('p_value')),
				~ 1 / .x
			))

		if ("conf_low" %in% est_var) {
			tbl <-
				dplyr::rename(tbl, conf_high = conf_low, conf_low = conf_high)
		}
	}

	# Modify table based on strata, terms, etc

	# Plot setup ----

	## Axis arguments
	x_vars <- formulas_to_named_list(axis)

	if ('lim' %in% names(x_vars)) {
		lim_val <- eval(str2lang(x_vars$lim))
		xmin <- min(lim_val)
		xmax <- max(lim_val)
	} else {
		xmin <- min(tbl$conf_low, na.rm = TRUE)
		xmax <- max(tbl$conf_high, na.rm = TRUE)
	}

	if ('int' %in% names(x_vars)) {
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

	if ('breaks' %in% names(x_vars)) {
		breaks <- eval(x_vars$breaks)
	} else {
		breaks <- ggplot2::waiver()
	}

	if ('lab' %in% names(x_vars)) {
		lab <- x_vars$lab
	} else {
		lab <- NULL
	}

	if ('scale' %in% names(x_vars)) {
		scale <- x_vars$scale
	} else if (unique(object$model_call) %in% c('glm', 'coxph')) {
		scale <- 'log'
	} else {
		scale <- 'continuous'
	}

	## Basic plots in table format
	# Will inject the general sizing of plots here
	# These options will scale with each other, and start and sensible default
	forestOptions <- formulas_to_named_list(forest)

	plotOptions <- list(
		size = 1,
		shape = 'circle',
		linetype = 3,
		linewidth = 1
	)

	for (i in names(forestOptions)) {
		plotOptions[[i]] <- forestOptions[[i]]
	}


	ptbl <-
		tbl |>
		dplyr::group_by(outcome, term, strata, level) |>
		tidyr::nest() |>
		dplyr::mutate(gg = purrr::map(data, ~ {
			ggplot(.x, aes(x = estimate, y = 0)) +
				geom_point(size = plotOptions$size * 30, shape = plotOptions$shape) +
				geom_linerange(aes(
					xmax = conf_high,
					xmin = conf_low,
					linewidth = plotOptions$linewidth
				)) +
				geom_vline(
					xintercept = xint,
					linetype = plotOptions$linetype,
					linewidth = plotOptions$linewidth * 5
				) +
				#theme_minimal() +
				theme_void() +
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
							trans = scales::pseudo_log_trans(sigma = 0.1, base = exp(1)),
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

	## Create axis at bottom
	tmp <- ptbl$gg[[1]]
	tmp$layers[1:2] <- NULL
	btm_axis <-
		tmp +
		xlab(lab) +
		theme(
			axis.text.x = element_text(margin = margin(10, 0 , 0 , 0), size = plotOptions$size * 50),
			axis.ticks.x = element_line(linewidth = plotOptions$size * 5),
			axis.ticks.length.x = unit(30, "pt"),
			axis.title.x = element_text(margin = margin(10, 0, 0 , 0), size = plotOptions$size * 100),
			axis.line.x = element_line(
				linewidth = plotOptions$size * 5,
				arrow = grid::arrow(
					length = grid::unit(50, "pt"),
					ends = "both",
					type = "closed"
				),
				colour = 'black'
			)
		)

	ptbl$gg[nrow(ptbl)] <- list(btm_axis)

	# As we will be whiting out every other row, will need a masking layer
	# Will pick hte "lowest level" in each strata to "white out"
	# To use to help filter for which variables to modify in grouped rows
	masking_lvls <-
		subset(tbl, select = c(strata, level)) |>
		dplyr::group_by(strata) |>
		dplyr::mutate(mask = dplyr::if_else(level == min(level), FALSE, TRUE)) |>
		dplyr::pull(mask)

	## Re-create table, adding in parallel positions for plots
	# Will need to "organize it" based on how many strata and terms there are
	# Strata x 1, Terms x >1 = Grouped by Term (no need for strata)
	# Strata x >1, Terms x 1 = Grouped by Strata (no need for term)
	# Default = Group by Term
	ftbl <-
		tbl |>
		# Rename or relabel components
		dplyr::group_by(outcome, term) |>
		#dplyr::mutate(level = lvl_lab) |>
		dplyr::rowwise() |>
		dplyr::mutate(
			level = lvl[[as.character(level)]],
			strata = sta[[strata]],
			term = tms[[term]],
			outcome = out[[outcome]]
		) |>
		dplyr::ungroup() |>
		dplyr::mutate(ggplots = NA) |>
		dplyr::add_row() |>
		# Place in correct columnar order
		dplyr::select(term,
									strata,
									level,
									any_of(mod_var),
									any_of(est_var),
									ggplots)

	if (length(sta) == 1 & length(tms) > 1) {
		rowCol <- 'level'
		groupCol <- 'term'
		ftbl <-
			subset(ftbl, select = -strata)
	} else if (length(sta) >= 1 & length(tms) == 1) {
		rowCol <- 'level'
		groupCol <- 'strata'
		ftbl <-
			subset(ftbl, select = -term)
	} else {
		rowCol <- 'level'
		groupCol <- 'strata'
	}

	## Convert to a `gt` table here and convert plots
	# Variable that are meant to fine tune the graph are evaluated here

	colWidths <-
		formulas_to_named_list(width) |>
		lapply(as.numeric)
	if (is.null(colWidths$n)) {
		colWidths$n <- 0.1
	}
	if (is.null(colWidths$beta)) {
		colWidths$beta <- 0.4
	}
	if (is.null(colWidths$forest)) {
		colWidths$forest <- 0.4
	}

	gtbl <-
		ftbl |>
		gt(rowname_col = rowCol, groupname_col = groupCol) |>
		# Estimates and confidence intervals
		{\(.) {
			if (all(c("estimate", "conf_low", "conf_high") %in% est_var)) {
				. |>
					cols_merge(columns = est_var[1:3],
										 pattern = "{1} ({2}, {3})") |>
					cols_width(estimate ~ pct(colWidths$beta * 100)) |>
					cols_label(estimate = cols$beta)
			} else {
				.
			}
		}}() |>
		# Number of observations
		{\(.) {
			if (all(c("nobs") %in% mod_var)) {
				. |>
					cols_width(nobs ~ pct(as.numeric(colWidths$n * 100))) |>
					cols_label(nobs = cols$n)
			} else {
				.
			}
		}}() |>
		# P value included for general groups
		{\(.) {
			if (all(c("p.value") %in% est_var)) {
				. |>
					cols_move_to_end(p_value) |>
					tab_style(
						style = cell_text(weight = "bold"),
						locations = cells_body(columns = p_value,
																	 rows = p_value < 0.05)
					) |>
					cols_label(p_value = cols$p)
			} else {
				.
			}
		}}() |>
		# Control digits and significant figures
		fmt_number(
			columns = where(is.numeric),
			drop_trailing_zeros = TRUE,
			n_sigfig = 2
		) |>
		tab_style(
			style = list(
				cell_borders(sides = "all", color = NULL)
			),
			locations = list(
				cells_body(columns = c(all_of(mod_var), all_of(est_var))),
				cells_stub(rows = everything())
			)
		) |>
		#cols_width(ggplots ~ pct(as.numeric(colWidths$forest * 100))) |>
		opt_vertical_padding(scale = 0) |>
		opt_table_outline(style = "none") |>
		tab_options(
			data_row.padding = px(0),
			table_body.border.bottom.width = px(0),
			table_body.border.top.width = px(0),
			column_labels.border.top.width = px(0)
		) |>
		tab_style(
			style = list(
				cell_text(color = "white", size = px(0)),
				cell_borders(sides = "all", color = NULL)
			),
			locations = list(
				cells_body(columns = ggplots),
				cells_row_groups(groups = "NA"),
				cells_stub(rows = is.na(level))
			)
		) |>
		tab_style(
			style = list(
				cell_text(color = "white", size = px(0))
			),
			locations = list(
				cells_body(columns = c(all_of(mod_var), all_of(est_var)),
									 rows = is.na(level))
			)
		) |>
		# Modification of ggplot
		cols_label(
			ggplots = x_vars$title,
		) |>
		text_transform(
			locations = cells_body(columns = ggplots),
			fn = function(x) {
				purrr::map(ptbl$gg,
									 ggplot_image,
									 height = px(50),
									 aspect_ratio = colWidths$forest * 10)
			}
		)


}
