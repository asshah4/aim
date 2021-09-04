
#' Make a table of sequentially fit models using `gt`
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' This function is a wrapper for the `gt` package for quickly and easily making
#' model tables. It is built for sequentially adjusted models that have been
#' created from the [dagger::extract_models()] function, which is primarily uses
#' the [broom::tidy()] function to help describe model fits.
#'
#' @param x A data table with columns that are similar to that from
#'   [broom::tidy()], along with an naming variable that identifies which model
#'   the data is coming from. The default options presume that the data was
#'   generated from [broom::tidy()] with confidence intervals included.
#'
#' @param var_col Character name of the column containing the independent
#'   variable terms.
#'
#' @param var_list Named list of the primary exposure variables that were
#'   contained within each model, along with a text description (e.g. `list(x =
#'   "primary_variable")`).
#'
#' @param group_var Defaults to NULL. If present, presumes a grouping variable
#'   or level of analyses were present. Each group is given the label that
#'   represents the value of the level. To be more informative, can relabel the
#'   values prior to passing to the function.
#'
#' @param rank_col Character name of the column that serves as the ordering
#'   variable for the models, such as a sequence in the setting of sequential
#'   modeling.
#'
#' @param ranks Vector containing which models to select (either numerically or
#'   by a name), based on user preference. Serves as the row names in the `gt`
#'   table.
#'
#' @param rank_lab Defaults to NULL. Character string that would be prepended to
#'   each individual rank to provide legibility to the rows. For example, if
#'   `ranks = c(1, 2, 3)`, then `rank_lab = "Model"` would produce the labels
#'   "Model 1", "Model 2", "Model 3". If missing, will not modify the row names.
#'
#' @param disp_col The numeric columns that contain the variables that should be
#'   displayed, such as `disp_col = c("estimate", "conf.low", "conf.high")`,
#'   which will be merged together in format chosen by the __disp_glue__
#'   argument.
#'
#' @param disp_glue A character vector that use the [glue::glue()] package to
#'   describe how the display columns should be arranged, such as `disp_glue =
#'   "{1} ({2}, {3})"`. The numbers represent the corresponding position in the
#'   vector given to the __disp_col__ argument (e.g. "{1}" represents
#'   "estimate").
#'
#' @param stat_col Occasionally the individual cells should be modified to show
#'   significance or importance of findings. This argument is a single character
#'   vector to name which column will help qualify that vector. For example,
#'   "p.value" may be used to help decide significance. Defaults to NA.
#'
#' @param stat_val If a statistic is chosen from the __stat_col__ argument, then
#'   it is compared with and expected to be lower than the __stat_val__ argument
#'   (`stat_col < stat_val`).
#'
#' @param stat_style If a statistic is chosen, then `gt` offers the
#'   [gt::tab_style()] function, which allows for modification. The options are
#'   `c("fill", "text", "borders")`, which correspond to `cell_fill()`,
#'   `cell_text()`, and `cell_borders()`.
#'
#' @param stat_opts If a statistic is chosen, then a named list, such as
#'   `list(color = "lightgreen")` can be given to be used for the __stat_style__
#'   argument. There is not a validation method for this, so please read
#'   documentation from `gt` for this function.
#'
#' @param decimals The number of decimals to be used for displaying output.
#'   Defaults to 2 decimal places.
#'
#' @param ... For passing additional arguments
#'
#' @import gt
#' @importFrom dplyr filter mutate
#' @importFrom rlang := .data
#' @export
tbl_sequential <- function(x,
													 var_col = "term",
													 var_list,
													 group_var = NULL,
													 rank_col = "number",
													 ranks,
													 rank_lab = NULL,
													 disp_col = c("estimate", "conf.low", "conf.high"),
													 disp_glue = "{1} ({2}, {3})",
													 stat_col = NA,
													 stat_val = 0.05,
													 stat_style = "fill",
													 stat_opts = list(color = "lightgreen"),
													 decimals = 2,
													 ...) {

	# Data table of level, number, term, estimate, statistic, p.value, conf.low, conf.high
	# Main variables
	vars <- names(var_list)

	# If grouping variables are present
	if (!is.null(group_var)) {
		n <- length(unique(x[[group_var]]))
	} else {
		n <- 1
	}

	# Ranks
	if (!is.null(rank_lab)) {
		rank_lab <- rep(paste(rank_lab, 1:length(ranks)), n)
		rank_col_sym <- rlang::sym(rank_col)
	} else {
		rank_lab <- as.character(ranks)
	}

	# Create initial `gt` model
	y <-
		x %>%
		dplyr::filter( .data[[var_col[[1]]]] %in% vars) %>%
		dplyr::select(dplyr::any_of(c({{ group_var }}, {{ rank_col }}, {{ var_col }}, {{ disp_col }}, {{ stat_col }}))) %>%
		dplyr::filter( .data[[rank_col[[1]]]] %in% ranks) %>%
		tidyr::pivot_wider(
			names_from = .data[[var_col[[1]]]],
			values_from = c({{ disp_col }}, {{ stat_col }}),
			names_glue = paste0("{", var_col[[1]], "}_{.value}")
		) %>%
		dplyr::mutate(!!rank_col_sym := rank_lab) %>%
		{
			if (!is.null(group_var)) {
				gt(., rowname_col = rank_col, groupname_col = group_var)
			} else gt(., rowname_col = rank_col)
		}

	# Merge columns
	for (i in vars) {
		y <-
			y %>%
			cols_merge(columns = starts_with(i), pattern = disp_glue)
	}

	# Column names
	names(var_list) <- paste0(vars, "_estimate")
	y <-
		y %>%
		cols_label(.list = var_list)

	# Significant values and styling
	if (!is.na(stat_col)) {

		stat_names <-
			paste0(vars, "_", stat_col) %>%
			lapply(., rlang::sym)
		var_names <-
			names(var_list) %>%
			lapply(., rlang::sym)

		for (i in 1:length(vars)) {
			y <-
				y %>%
				tab_style(
					style = switch(
						stat_style,
						fill = list(cell_fill(stat_opts))
					),
					locations = cells_body(
						columns = !!var_names[[i]],
						rows = !!stat_names[[i]] < stat_val
					)
				)
		}
	}

	# Final touchups
	y <-
		y %>%
		fmt_number(columns = everything(), decimals = decimals)

	y

}
