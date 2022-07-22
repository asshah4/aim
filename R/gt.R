#' Make a table of sequentially fit models using `gt`
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' This function is a wrapper for the `gt` package for quickly and easily making
#' model tables. It is built for sequentially adjusted models that have been
#' created from the [murmur::extract_results()] function, which is primarily uses
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
#' @param models A _formula ~ list_ pattern where the left-hand side indicates
#'   the column in __data__ that contains the model identifiers, and the
#'   right-hand side indicates which models should be used. If it is given as a
#'   named list, then the models will be relabeled when displayed.
#'
#'   For example, `model_id ~ list(1, 3, 5)` would select the models labeled as
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
#' @param by This can either be a _string_ or a _formula ~ list_ pattern.
#'   Defaults to NULL. If present, presumes a grouping variable or level of
#'   analyses were present. If a string is passed, then each group is given the
#'   label that represents the value of the level by default. If a _formula_ is
#'   passed, then the left-hand side represents the column that is the grouping
#'   variable, and the right-hand side is a named list that indicates what
#'   should be relabeled.
#'
#'   For example, `level ~ list("0" = "Empty", "1" = "Full")` would relabel the
#'   groups with "Empty" and "Full" for the corresponding levels.
#'
#' @param values The numeric columns that contain the variables that should be
#'   displayed, such as `values = c("estimate", "conf.low", "conf.high")`,
#'   which will be merged together in format chosen by the __pattern__
#'   argument. The first element is the primary variable to be displayed.
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
#' @importFrom dplyr filter mutate
#' @importFrom rlang := .data
#' @family visualizers
#' @export
tbl_sequential <- function(data,
													 terms,
													 models,
													 model_label = NULL,
													 model_notes = NULL,
													 by = NULL,
													 values = c("estimate", "conf.low", "conf.high"),
													 pattern = "{1} ({2}, {3})",
													 statistic = NULL,
													 style = fill ~ list(color = "lightgreen"),
													 decimals = 2,
													 ...) {

	if (!requireNamespace("gt", quietly = TRUE)) {
		stop(
			"Package \"gt\" needed for this function to work. Please install it.",
			call. = FALSE
		)
	}

	# Validation
	validate_class(terms, "formula")
	validate_class(models, "formula")
	validate_class(values, "character")

	# Columns to extract
	var_col_sym <- terms[[2]]
	var_col <- as.character(var_col_sym)
	model_col_sym <- models[[2]]
	model_col <- as.character(model_col_sym)
	if (!is.null(statistic)) {
		stat_col_sym <- statistic[[2]]
		stat_col <- as.character(stat_col_sym)
	} else {
		stat_col <- NULL
	}
	disp_col <- values
	if (!is.null(by)) {
		if (is.character(by)) {
			strata_col <- by
		} else if (class(by) == "formula") {
			strata_col_sym <- by[[2]]
			strata_col <- as.character(strata_col_sym)
		}
	} else {
		strata_col <- NULL
	}


	# Terms
	var_list <- eval(terms[[3]])
	var_names <- unname(unlist(var_list))
	vars <-
		lapply(seq_along(var_list), function(x) {
			if (is.null(names(var_list[x]))) {
				var_list[[x]]
			} else if (names(var_list[x]) == "") {
				var_list[[x]]
			} else {
				names(var_list[x])
			}
		}) %>%
		unlist()

	# Model selection
	model_list <- unlist(eval(models[[3]]))
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
		if (is.null(names(model_list))) {
			model_list
		} else if (is.vector(model_list)) {
			lapply(seq_along(model_list), function(x) {
				if (is.null(names(model_list[x]))) {
					model_list[x]
				} else if (names(model_list[x]) == "") {
					model_list[[x]]
				} else {
					names(model_list[x])
				}
			}) %>%
			unlist()
		}

	# If grouping variables are present, data can be modified
	if (!is.null(by)) {
		n <- length(unique(data[[strata_col]]))
		if (class(by) == "formula") {
			strata_val <- eval(by[[3]])
			strata_vec <- unlist(strata_val)
			data[[strata_col]] <-
				unname(strata_vec)[match(data[[strata_col]], names(strata_vec))]
		}
	} else {
		n <- 1
	}

	# Create initial `gt` model
	x <-
		data %>%
		dplyr::filter(.data[[var_col]] %in% vars) %>%
		dplyr::select(dplyr::any_of(c({{ strata_col }}, {{ model_col }}, {{ var_col }}, {{ disp_col }}, {{ stat_col }}))) %>%
		dplyr::filter(.data[[model_col]] %in% mods) %>%
		tidyr::pivot_wider(
			names_from = .data[[var_col]],
			values_from = c({{ disp_col }}, {{ stat_col }}),
			names_glue = paste0("{", var_col, "}_{.value}")
		) %>%
		dplyr::mutate(!!model_col_sym := rep(model_names, n)) %>%
		{
			if (!is.null(by)) {
				gt::gt(., rowname_col = model_col, groupname_col = strata_col)
			} else gt::gt(., rowname_col = model_col)
		}

	# Merge columns
	for (i in vars) {
		x <-
			x %>%
			gt::cols_merge(columns = gt::starts_with(i), pattern = pattern)
	}

	# Column names should be relabeled
	disp_val <- disp_col[1]
	var_gt <- var_list
	names(var_gt) <- paste0(vars, "_", disp_val)
	x <-
		x %>%
		gt::cols_label(.list = var_gt)

	# Significant values and styling
	if (!is.null(statistic)) {

		stat_val <- eval(statistic[[3]])

		stat_lab <-
			paste0(vars, "_", stat_col) %>%
			lapply(., rlang::sym)
		var_gt <-
			names(var_gt) %>%
			lapply(., rlang::sym)

		stat_style <- as.character(style[[2]])
		stat_opts <- unlist(eval(style[[3]]))

		for (i in 1:length(vars)) {
			x <-
				x %>%
				gt::tab_style(
					style = switch(
						stat_style,
						fill = list(gt::cell_fill(stat_opts))
					),
					locations = gt::cells_body(
						columns = !!var_gt[[i]],
						rows = !!stat_lab[[i]] < stat_val
					)
				)
		}
	}

	# Add Footnotes
	if (!is.null(model_notes)) {

		validate_class(model_notes, "list")
		if (length(model_notes) != (length(mods) + 1)) {
			"The `model_notes` argument is not of the correct length."
		}

		out <- model_notes[[1]]

		footnote_list <- list()

		for (i in 1:length(mods)) {

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
		for (i in 1:length(mods)) {
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
							locations = gt::cells_stub(rows = c(i, i + (j - 1)*length(mods)))
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

#' Make a table of models that are to be compared using `gt`
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' This function is a wrapper for the `gt` package for quickly and easily making
#' model tables. It is built for fully adjusted models that have been created
#' from the [murmur::extract_results()] function, which primarily uses the
#' [broom::tidy()] function to help describe model fits. The models should have
#' specific variables of interest that are different for comparison.
#'
#' @inheritParams tbl_sequential
#'
#' @param terms A _formula ~ list_ where the left-hand side indicates the column
#'   in __data__ and right-hand side indicates which variables are of interest,
#'   along with their potential label (thus a named list).
#'
#'   For example, `term ~ list(x = "primary")` would select the column named
#'   "term" and it would label variable "x" with the description "primary" for
#'   display in each row.
#'
#'   If instead, no name is given, such as `term ~ list("x")` it is presumed
#'   that the variables will go by their original name from the table.
#'
#'   This is slightly different than in [murmur::tbl_sequential()] in that these
#'   terms define which components to show in each row of the table. If there
#'   are non-unique terms (e.g. "primary", "secondary"), it is assumed that they
#'   should be grouped in the __by__ argument.
#'
#' @param by This can either be a _string_ or a _formula ~ list_ pattern.
#'   Defaults to NULL. If present, presumes that there are non-unique __terms__
#'   selected from above, and that they should be grouped as an individual
#'   analysis.
#'
#'   If a _string_ is passed, then each group is given the label that represents
#'   the value of the level by default.
#'
#'   If a _formula_ is passed, then the left-hand side represents the column
#'   that is the grouping variable, and the right-hand side is a named list that
#'   indicates what should be relabeled. This also allows to limit which
#'   groups/analyses should be included, as unnamed/unlisted levels will be
#'   ignored.
#'
#'   For example, `analysis ~ list("1" = "Before", "2" = "After")` would relabel
#'   the groups with "Empty" and "Full" for the corresponding levels.
#'
#' @param missing_text A string used to fill in missing values, which may occur in
#'   comparison of models that do not have all the same terms or could not be
#'   analyzed.
#'
#' @importFrom dplyr filter mutate
#' @importFrom rlang := .data
#' @family visualizers
#' @export
tbl_compare <- function(data,
												terms,
												by = NULL,
												models,
												values = c("estimate", "conf.low", "conf.high"),
												pattern = "{1} ({2}, {3})",
												statistic = NULL,
												style = fill ~ list(color = "lightgreen"),
												decimals = 2,
												missing_text = NA,
												...) {

	# Validation
	if (!requireNamespace("gt", quietly = TRUE)) {
		stop(
			"Package \"gt\" needed for this function to work. Please install it.",
			call. = FALSE
		)
	}

	# Validation
	validate_class(terms, "formula")
	validate_class(models, "formula")
	validate_class(values, "character")

	# Columns to extract
	var_col_sym <- terms[[2]]
	var_col <- as.character(var_col_sym)
	model_col_sym <- models[[2]]
	model_col <- as.character(model_col_sym)
	if (!is.null(statistic)) {
		stat_col_sym <- statistic[[2]]
		stat_col <- as.character(stat_col_sym)
	} else {
		stat_col <- NULL
	}
	disp_col <- values
	if (!is.null(by)) {
		if (is.character(by)) {
			strata_col <- by
		} else if (class(by) == "formula") {
			strata_col_sym <- by[[2]]
			strata_col <- as.character(strata_col_sym)
			strata_vec <- unlist(eval(by[[3]]))
		}
	} else {
		strata_col <- NULL
	}

	# Terms
	var_list <- eval(terms[[3]])
	var_names <- unname(unlist(var_list))
	vars <-
		lapply(seq_along(var_list), function(x) {
			if (is.null(names(var_list[x]))) {
				var_list[[x]]
			} else if (names(var_list[x]) == "") {
				var_list[[x]]
			} else {
				names(var_list[x])
			}
		}) %>%
		unlist()

	# Model selection
	model_list <- unlist(eval(models[[3]]))
	model_names <- unlist(unname(model_list))
	mods <-
		if (is.null(names(model_list))) {
			model_list
		} else if (is.vector(model_list)) {
			lapply(seq_along(model_list), function(x) {
				if (is.null(names(model_list[x]))) {
					model_list[x]
				} else if (names(model_list[x]) == "") {
					model_list[[x]]
				} else {
					names(model_list[x])
				}
			}) %>%
			unlist()
		}

	# Create initial `gt` model
	x <-
		data %>%
		dplyr::filter(.data[[var_col]] %in% vars) %>%
		dplyr::select(dplyr::any_of(c({{ strata_col }}, {{ model_col }}, {{ var_col }}, {{ disp_col }}, {{ stat_col }}))) %>%
		dplyr::filter(.data[[model_col]] %in% mods) %>%
		{
			if (!is.null(strata_col))
				dplyr::filter(., .data[[strata_col]] %in% names(strata_vec))
			else .
		} %>%
		tidyr::pivot_wider(
			names_from = .data[[model_col]],
			values_from = c({{ disp_col }}, {{ stat_col }}),
			names_glue = paste0("{", model_col, "}_{.value}")
		) %>%
		dplyr::mutate(!!var_col_sym := factor(!!var_col_sym, levels = names(var_list), labels = unlist(unname(var_list)))) %>%
		{
			if (!is.null(strata_col))
				dplyr::mutate(., !!strata_col_sym := factor(!!strata_col_sym, levels = names(strata_vec), labels = unlist(unname(strata_vec))))
			else .
		} %>%
		{
			if (!is.null(by)) {
				gt::gt(., rowname_col = var_col, groupname_col = strata_col)
			} else gt::gt(., rowname_col = var_col)
		}

	# Merge columns
	for (i in mods) {
		x <-
			x %>%
			gt::cols_merge(columns = gt::starts_with(i), pattern = pattern)
	}

	# Column names should be relabeled
	disp_val <- disp_col[1]
	mods_gt <- model_list
	names(mods_gt) <- paste0(mods, "_", disp_val)
	x <-
		x %>%
		gt::cols_label(.list = mods_gt)

	# Significant values and styling
	if (!is.null(statistic)) {

		stat_val <- eval(statistic[[3]])

		stat_lab <-
			paste0(mods, "_", stat_col) %>%
			lapply(., rlang::sym)
		mods_gt <-
			names(mods_gt) %>%
			lapply(., rlang::sym)

		stat_style <- as.character(style[[2]])
		stat_opts <- unlist(eval(style[[3]]))

		for (i in 1:length(mods)) {
			x <-
				x %>%
				gt::tab_style(
					style = switch(
						stat_style,
						fill = list(gt::cell_fill(stat_opts))
					),
					locations = gt::cells_body(
						columns = !!mods_gt[[i]],
						rows = !!stat_lab[[i]] < stat_val
					)
				)
		}
	}

	# TODO Add Footnotes

	# Final touchups with missing values and decimals (opinionated)
	x <-
		x %>%
		gt::fmt_number(columns = gt::everything(), decimals = decimals) %>%
		gt::fmt_missing(columns = gt::everything(), missing_text = missing_text)

	# Return
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

#' Make a `gt` table for many models
#'
#' @param object An object that represents multiple models. These should be of the same type (e.g. linear, survival, logistic, etc) for interpretability. The accepted format for this argument is as below:
#'
#'   * `data.frame` An unnested/simple table that contains identifiers for multiple models that are thematically related (based on exposures, model types, etc)
#'
#'   * `model_map` An object created from [murmur::create_models()] that has been fitted using [murmur::construct_tests()]
#'
#'   * `list` A list of models that are related
#'
#' @param x A variable that indicates the primary exposure. This may or may not be shared amongst models. This can be either a _character vector_ or a _formula ~ list_ object. See the __details__ section below for further information.
#'
#' @param direction A character vector `c("horizontal", "vertical")`. If the
#'   table should be oriented horizontally, with outcomes being columns, or if
#'   the table should be oriented vertically, with outcomes being rows.

#' @param style If a __statistic__ was chosen, then specific options can be
#'   passed to help emphasize those particular cells. This is a _formula ~ list_
#'   object where the left-hand side indicates the type of style to choose, and
#'   the right-hand side indicates the implementation. These are based on the
#'   [[gt::tab_style()]] function.
#'
#'   Left-hand side should be one of _fill_, _text_, or _borders_, which
#'   correspond to to `cell_fill()`, `cell_text()`, and `cell_borders()`. The
#'   right-hand side is a named list such as `list(color = lightgreen)`, and
#'   accepts options described by the `gt` package. There is no validation for
#'   the options at this time, so see documentation the `gt` package for
#'   details.

#' @param ... For passing additional arguments
#'
#' @details
#'
#' The `id`, `x`, `y`, and `cov` arguments can take several argument types. They
#' are intended to be used to help select the variable (or column), subset that
#' data, and re-label the data for display (if needed).
#'
#'   * A _character vector_ that names either the column or location containing
#'   the variable, or the name of the variable itself (depending on the context
#'   of method dispatch).
#'
#'   * A _formula ~ list_ where the LHS indicates the column or location of the
#'   data, and the RHS indicates the variables themselves. If the RHS list is a
#'   named list, it will use the name as the variable and the value for display.
#'   For example, `term ~ list(x = "Exposure")` would represent a location
#'   called _term_, with the major variable being _x_, and the displayed value
#'   being _Exposure_. This also allows subsets, as it will only retain the
#'   variables that are listed.
#'
#' @importFrom dplyr filter mutate
#' @importFrom rlang := .data
#' @family visualizers
#' @export
tbl_models <- function(object, ...) {
	UseMethod("tbl_models", object = object)

	# Key variables
	#
	# 	- Overall model identifier
	# 	- Major outcome
	# 	- Major exposure
	# 	- Minor variables/covariates
	#		- Statistics to be displayed

	# The key variables have a rank/hierarchy to them
	# Ordering variables:
	#
	# 	1. id = Model identifiers
	# 	2. x = Exposures
	# 	3. y = Outcomes
	# 	4. cov = Covariates
	# 	5. values = Value columns
}

#' @export
tbl_models.data.frame <- function(object,
																	id,
																	x,
																	y,
																	terms,
																	values,
																	direction,
																	...) {

	if (!requireNamespace("gt", quietly = TRUE)) {
		stop(
			"Package \"gt\" needed for this function to work. Please install it.",
			call. = FALSE
		)
	}

	# Data and term preparation ----
	validate_class(object, "data.frame")

	# Model ID
	if (class(id) == "character" & length(id == 1)) {
		id_col <- return_cols(id)
		id_lab <- id_var <- unique(object[[id_col]])
	} else {
		id_col <- return_cols(id)
		id_var <- return_rows(id)
		id_lab <- return_names(id)
	}

	# Independent variable
	if (class(x) == "character" & length(x == 1)) {
		x_col <- return_cols(x)
		x_lab <- x_var <- unique(object[[x_col]])
	} else {
		x_col <- return_cols(x)
		x_var <- return_rows(x)
		x_lab <- return_names(x)
	}

	# Dependent variable
	if (class(y) == "character" & length(y == 1)) {
		y_col <- return_cols(y)
		y_lab <- y_var <- unique(object[[y_col]])
	} else {
		y_col <- return_cols(y)
		y_var <- return_rows(y)
		y_lab <- return_names(y)
	}

	# Terms
	if (class(terms) == "character" & length(terms == 1)) {
		t_col <- as.symbol(terms)
		t_lab <- t_var <- unique(object[[t_col]])
	} else {
		t_col <- return_cols(terms)
		t_var <- return_rows(terms)
		t_lab <- return_names(terms)
	}

	# Values
	val_col <- sapply(values, FUN = as.symbol, USE.NAMES = FALSE)

	# Statistic
	if (!is.null(statistic)) {
		validate_class(statistic, "formula")
		stat_col <- return_cols(statistic)
		stat_val <- eval(statistic[[3]])
	}

	# Data creation
	data <-
		object |>
		dplyr::filter(.data[[id_col]] %in% id_var) |>
		dplyr::filter(.data[[x_col]] %in% x_var) |>
		dplyr::filter(.data[[y_col]] %in% y_var) |>
		dplyr::filter(.data[[t_col]] %in% t_var) |>
		dplyr::select(dplyr::any_of(c(
			as.character(id_col),
			as.character(x_col),
			as.character(y_col),
			as.character(t_col),
			as.character(val_col),
			as.character(stat_col)
		))) |>
		dplyr::mutate(!!t_col := factor(!!t_col, levels = t_var, labels = t_lab))

	# Horizontal Table ----
	if (direction == "horizontal") {

		tab <-
			data |>
			tidyr::pivot_wider(
				names_from = as.character(y_col),
				values_from = as.character(c(val_col, stat_col)),
				names_glue = paste0("{", y_col, "}_{.value}")
			) |>
			gt::gt(
				rowname_col = as.character(t_col),
				groupname_col = as.character(x_col)
			)

		# Merge columns
		for (i in y_var) {
			tab <-
				tab |>
				gt::cols_merge(columns = gt::starts_with(i), pattern = pattern)
		}

		# Rename columns
		names(y_lab) <- paste0(y_var, "_", val_col[[1]])
		tab <- gt::cols_label(tab, .list = y_lab)

		# Significant values and styling
		if (!is.null(statistic)) {
			stat_lab <-
				paste0(y_var, "_", stat_col) |>
				lapply(rlang::sym)

			stat_style <- as.character(style[[2]])
			stat_opts <- unlist(eval(style[[3]]))

			for (i in 1:length(y_var)) {
				tab <-
					tab |>
					gt::tab_style(
						style = switch(
							stat_style,
							fill = list(gt::cell_fill(stat_opts))
						),
						locations = gt::cells_body(
							columns = !!names(y_lab)[[i]],
							rows = !!stat_lab[[i]] < stat_val
						)
					)
			}
		}

		# Missing values and decimals (opinionated)
		tab <-
			tab %>%
			gt::fmt_number(columns = gt::contains(as.character(val_col[[1]])), decimals = decimals) %>%
			gt::fmt_missing(columns = gt::everything(), missing_text = missing_text)
	}


	# Return
	tab

}


#' Subgroup Forest Plot
#'
#' @param object Class `forge` object with models that have been fit
#'
#' @param columns Additional columns that help to describe the subgroup models.
#'   At least one column should be selected from this list. The current options
#'   are:
#'
#'   * beta = point estimate value, such as odds ratio or hazard ratio
#'
#'   * conf = inclusion of the confidence interval (presumed to be 95%-ile)
#'
#'   * n = number of observations in each model group
#'
#'   For example: `list(beta ~ "Hazard", conf ~ "95% CI" n ~ "No.")"`
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
#'   For example: `list(title ~ "Decreasing Hazard", lab ~ "HR (95% CI))`
#'
#' @import ggplot2
#' @export
tbl_forest <- function(object, x) {
	UseMethod("tbl_forest", object = x)
}

#' @export
tbl_forest.character <- function(object,
																 y,
																 x,
																 groups = character(),
																 columns = list(beta ~ "Estimate",
																 							 conf ~ "95% CI",
																 							 n ~ "No."),
																 axis = list()) {

	# Validate
	if (!inherits(object, "forge")) {
		stop("Object should be of type `forge`, not `", class(object)[1], "`")
	}
	cols <- formula_to_named_list(columns)

	# Rename selecting columns (for both parameter estimates and model info)
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
	basic <-
		object |>
		dplyr::filter(strata %in% groups) |>
		dplyr::filter(outcome == y & exposure == x) |>
		dplyr::as_tibble()

	est <-
		basic$parameter_estimates |>
		dplyr::bind_rows(.id = "level") |>
		dplyr::filter(term == x) |>
		dplyr::select(level, term, all_of(est_vars))

	inf <-
		basic$model_info |>
		dplyr::bind_rows(.id = "level") |>
		dplyr::select(level, all_of(mod_vars))

	tbl <-
		dplyr::full_join(est, inf, by = "level") |>
		dplyr::mutate(level = basic$level) |>
		dplyr::mutate(strata = basic$strata) |>
		dplyr::mutate(terms = basic$terms)

	# Setup plotting variables
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
			xmin > 0 & xmax < 1 ~ 0,
			xmin > 0 & xmax > 1 ~ 1,
			xmin < 0 & xmax > 0 ~ 0,
			xmin < 0 & xmax < 0 ~ 0,
			xmin < -1 & xmax < 0 ~ -1
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


	# Make appropriate plots
	plots <-
		tbl |>
		dplyr::add_row() |>
		dplyr::group_by(strata, level) |>
		tidyr::nest() |>
		dplyr::mutate(gg = purrr::map(data, ~ {
			ggplot(.x, aes(x = estimate, y = 0)) +
				geom_point(size = 50) +
				geom_linerange(aes(xmax = conf.high, xmin = conf.low, size = 5)) +
				geom_vline(xintercept = 0, linetype = 3, size = 5) +
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
				scale_x_continuous(breaks = breaks,
													 limits = c(xmin, xmax),
													 oob = scales::oob_squish)
				#coord_cartesian(xlim = c(-1, 6), ylim = c(-0.1, 0.1), clip = "off")
		})) |>
		tidyr::unnest(data) |>
		dplyr::ungroup()

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
			strata %in% names(labels(terms)) ~ labels(terms)[[strata]]
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
		gt::cols_width(ggplots ~ gt::px(300)) |>
		gt::cols_width(estimate ~ gt::px(300)) |>
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
