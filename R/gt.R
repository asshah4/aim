#' Extension of `gt` package for multiple models
#'
#' @param object A `mdl_tbl` object with the required models
#'
#' @param data The original dataset used to create the models of interest. The
#'   variables in the model must be contained within this dataset.
#'
#' @param outcomes Is a formula or list of formulas selecting the outcome
#'   variables of interest. The LHS is always the name of the variable that will
#'   be selected. The RHS is the potential label for the output table. If no
#'   label is desired, can place an `NA` value on RHS of formula.
#'
#' @param terms Is a `formula` or `list` of formulas selecting the model terms that
#'   should be used. The LHS is always the name of the variable that will be
#'   selected. The RHS is the potential label for the output table. If no label
#'   is desired, can place an `NA` value on RHS of formula.
#'
#' @name tbls
NULL

#' Compact and minimal theme for `gt` tables
#'
#' This theme was used for placing somewhat larger tables into `xaringan` slides
#' by making the spacing more compact and decreasing the font size. The exposed
#' variables are to control font size and table width, but any option from the
#' `gt` package is allowed.
#'
#' @inheritParams gt::tab_options
#' @param ... For passing additional arguments to the [tab_options()]
#'   function
#' @family visualizers
#' @importFrom gt tab_options px pct
#' @export
theme_gt_compact <- function(data,
														 table.font.size = pct(80),
														 table.width = pct(90),
														 ...) {

	validate_class(data, "gt_tbl")

	data |>
		tab_options(
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
