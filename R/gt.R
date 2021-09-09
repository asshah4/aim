
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
#' @param data A data table with columns that are similar to that from
#'   [broom::tidy()], along with an naming variable that identifies which model
#'   the data is coming from. The default options presume that the data was
#'   generated from [broom::tidy()] with confidence intervals included.
#'
#' @param terms A _formula ~ list_ where the left-hand side indicates the column
#'   in __data__ and right-hand side indicates which variables are of interest,
#'   along with their potential label (thus a named list).
#'
#'   For example, `term ~ list(x = "primary")` would select the column named
#'   "term" and it would label variable "x" with the description "primary" for
#'   display in the column headings.
#'
#'   If instead, no name is given, such as `term ~ list("x")` it is presumed
#'   that the variables will go by their original name from the table.
#'
#'
#' @param models A _formula ~ list_ pattern where the left-hand side indicates
#'   the column in __data__ that contains the model identifiers, and the
#'   right-hand side indicates which models should be used. If it is given as a
#'   named list, then the models will be relabeled when displayed.
#'
#'   For example, `model_id ~ list(1, 3, 5)` would select the models labelled as
#'   "1", "3", and "5". These would be re-sequenced as "1", "2", "3" for
#'   simplicity.
#'
#'   If a named list is given, such as `model_id ~ list(m = "Unadjusted")`, they
#'   will not be re-sequenced. If the model identifier is numeric, then it must
#'   be treated as a character when placed in a named list if it is being
#'   labeled, such as `model_id ~ list("1" = "Unadjusted")`.
#'
#' @param model_label Character vector for naming the models. If the models are
#'   not labeled as above, then this optional argument can be used to prefix the
#'   sequence of variables. The default is NULL, which retains the original
#'   model identifiers instead.
#'
#' @param model_notes This is an unnamed `list` object that allows for simple
#'   labeling of the how the sequential models are built. This list should be
#'   `length(models) + 1`. The first term is required to be the outcome
#'   variable. Subsequent list positions represent the additional terms that
#'   were included for sequential adjustment. There should be an item for each
#'   position selected by the __models__ argument.
#'
#'   For example:
#'
#'   `model_notes = list("outcome", "exposure", "x1", c("x2", "x3"))`
#'
#'   \eqn{Model 1 = outcome ~ exposure}
#'
#'   \eqn{Model 2 = outcome ~ Model 1 + x1}
#'
#'   \eqn{Model 3 = outcome ~ Model 2 + x2 + x3}
#'
#' @param by Defaults to NULL. If present, presumes a grouping variable
#'   or level of analyses were present. Each group is given the label that
#'   represents the value of the level. To be more informative, can relabel the
#'   values prior to passing to the function.
#'
#' @param values The numeric columns that contain the variables that should be
#'   displayed, such as `values = c("estimate", "conf.low", "conf.high")`,
#'   which will be merged together in format chosen by the __pattern__
#'   argument.
#'
#' @param pattern A string that uses [glue::glue()] to describe how the display
#'   columns should be arranged. With `pattern = "{1} ({2}, {3})"`, numbers
#'   represent the corresponding position in the vector given to the __pattern__
#'   argument (e.g. "{1}" represents "estimate").
#'
#' @param statistic A _formula_ object where the left-hand side indicates the
#'   statistic column of interest, and the right-hand side is the cut-off value
#'   for that parameter. For example, `statistic = p.value ~ 0.05` would select
#'   the "p.value" column and identify which cells had value under 0.05.
#'   Defaults to NULL.
#'
#' @param style If a __statistic__ was chosen, then specific options can be
#'   passed to help emphasize those particular cells. This is a _formula ~ list_
#'   object where the left-hand side indicates the type of style to choose, and
#'   the right-hand side indicates the implementation. These are based on the
#'   [[gt::tab_style()]] function.
#'
#'   Left-hand side should be one of c(fill, text, borders), which correspond to
#'   to `cell_fill()`, `cell_text()`, and `cell_borders()`. The right-hand side
#'   is a named list such as `list(color = lightgreen)`, and accepts options
#'   described by the `gt` package. There is no validation for the options at
#'   this time, so see documentation the `gt` package for details.
#'
#' @param decimals The number of decimals to be used for displaying output.
#'   Defaults to 2 decimal places.
#'
#' @param ... For passing additional arguments
#'
#' @import gt
#' @importFrom dplyr filter mutate
#' @importFrom rlang := .data
#' @family visualizers
#' @export
tbl_sequential <- function(data,
													 terms,
													 models,
													 model_label = NULL,
													 by = NULL,
													 values = c("estimate", "conf.low", "conf.high"),
													 pattern = "{1} ({2}, {3})",
													 statistic = NULL,
													 style = fill ~ list(color = "lightgreen"),
													 decimals = 2,
													 footnotes = NULL,
													 ...) {



	# Terms
	validate_class(terms, "formula")
	var_col_sym <- terms[[2]]
	var_col <- as.character(var_col_sym)
	var_list <- eval(terms[[3]])
	var_names <- unname(unlist(var_list))
	vars <-
		lapply(seq_along(var_list), function(x) {
			if (names(var_list[x]) == "") {
				var_list[[x]]
			} else {
				names(var_list[x])
			}
		}) %>%
		unlist()


	# Model selection
	validate_class(models, "formula")
	model_col_sym <- models[[2]]
	model_col <- as.character(model_col_sym)
	model_list <- eval(models[[3]])
	model_names <-
		if (is.null(names(model_list))) {
			if (is.null(model_label)) {
				1:length(model_list)
			} else {
				paste(model_label, 1:length(model_list))
			}
		} else {
			if (is.null(model_label)) {
				unlist(unname(model_list))
			} else {
				paste(model_label, unlist(unname(model_list)))
			}
		}
	mods <-
		lapply(seq_along(model_list), function(x) {
			if (names(model_list[x]) == "") {
				model_list[[x]]
			} else {
				names(model_list[x])
			}
		}) %>%
		unlist()

	# If grouping variables are present
	if (!is.null(by)) {
		n <- length(unique(data[[by]]))
	} else {
		n <- 1
	}

	# Create initial `gt` model
	x <-
		data %>%
		dplyr::filter(.data[[var_col]] %in% vars) %>%
		dplyr::select(dplyr::any_of(c({{ by }}, {{ model_col }}, {{ var_col }}, {{ disp_col }}, {{ stat_col }}))) %>%
		dplyr::filter( .data[[model_col]] %in% mods) %>%
		tidyr::pivot_wider(
			names_from = .data[[var_col]],
			values_from = c({{ disp_col }}, {{ stat_col }}),
			names_glue = paste0("{", var_col, "}_{.value}")
		) %>%
		dplyr::mutate(!!model_col_sym := rep(model_names, n)) %>%
		{
			if (!is.null(by)) {
				gt::gt(., rowname_col = model_col, groupname_col = by)
			} else gt::gt(., rowname_col = model_col)
		}

	# Merge columns
	for (i in vars) {
		x <-
			x %>%
			gt::cols_merge(columns = gt::starts_with(i), pattern = pattern)
	}

	# Column names
	var_lab <- var_list
	names(var_lab) <- paste0(vars, "_estimate")
	x <-
		x %>%
		gt::cols_label(.list = var_lab)

	# Significant values and styling
	if (!is.null(statistic)) {

		stat_col <- statistic[[2]]
		stat_val <- statistic[[3]]

		stat_lab <-
			paste0(vars, "_", stat_col) %>%
			lapply(., rlang::sym)
		var_lab <-
			names(var_lab) %>%
			lapply(., rlang::sym)

		stat_style <- as.character(style[[2]])
		stat_opts <- eval(style[[3]])

		for (i in 1:length(vars)) {
			x <-
				x %>%
				gt::tab_style(
					style = switch(
						stat_style,
						fill = list(gt::cell_fill(unlist(stat_opts)))
					),
					locations = gt::cells_body(
						columns = !!var_lab[[i]],
						rows = !!stat_lab[[i]] < stat_val
					)
				)
		}
	}

	# Add Footnotes
	if (!is.null(model_notes)) {

		validate_class(model_notes, "list")
		if (length(model_notes) != (length(models) + 1)) {
			"The `model_notes` argument is not of the correct length."
		}

		out <- model_notes[[1]]

		footnote_list <- list()

		for (i in 1:length(models)) {

			lhs <- paste(model_names[[i]], "=", out, "~")

			if (i == 1) {
				footnote_list[[i]] <-
					paste(model_notes[[i + 1]], collapse = " + ") %>%
					paste(lhs, .)
			} else {
				footnote_list[[i]] <-
					paste(model_notes[[i + 1]], collapse = " + ") %>%
					paste(model_names[[i - 1 ]], ., sep = " + ") %>%
					paste(lhs, .)
			}

		}

		# Add them to original structure
		for (i in 1:length(models)) {
			if (n == 1) {
				x <-
					x %>%
					gt::tab_footnote(
						footnote_list[[i]],
						locations = gt::cells_stub(rows = i)
					)
			}
			if (n > 1) {
				for (j in 2:n) {
					x <-
						x %>%
						gt::tab_footnote(
							footnote_list[[i]],
							locations = gt::cells_stub(rows = c(i, i + (j - 1)*length(models)))
						)
				}
			}
		}

	}

	# Final touchups
	x <-
		x %>%
		gt::fmt_number(columns = gt::everything(), decimals = decimals)

	x

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
#' @importFrom gt px pct
#' @family visualizers
#' @export
theme_gt_compact <- function(data,
														 table.font.size = pct(80),
														 table.width = pct(90),
														 ...) {

	validate_class(data, "gt_tbl")

	data %>%
		gt::tab_options(
			# Preset
			table.margin.left = px(1),
			table.margin.right = px(1),
			row_group.padding = px(1),
			data_row.padding = px(1),
			footnotes.padding = px(1),
			source_notes.padding = px(1),
			stub.border.width = px(1),
			# User supplied
			table.width = table.width,
			table.font.size = table.font.size,
			...
		)

}
