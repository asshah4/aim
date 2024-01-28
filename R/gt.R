#' Extension of `gt` package for multiple models
#'
#' @param object A `<mdl_tbl>` object with the required models
#'
#' @param data The original dataset used to create the models of interest. The
#'   variables in the model must be contained within this dataset.
#'
#' @param outcomes A `<formula>` or list of formulas selecting the outcome
#'   variables of interest. The **LHS** is always the name of the variable that will
#'   be selected. The **RHS** is the potential label for the output table. If no
#'   label is desired, can place an `NA` value on right-side of formula.
#'
#' @param exposures A `<formula>` or list of formulas of strata that should be
#'   evaluated, with the **LHS** referring to the strata and the **RHS**
#'   referring to its label.
#'
#' @param interactions A `<formula>` or list of formulas of the interaction
#'   terms that should be evaluated, with the **LHS** referring to the term and
#'   the **RHS** referring to its label. Currently only supports binary
#'   interaction terms.
#'
#' @param strata A `<formula>` or list of formulas of strata that should be
#'   evaluated, with the **LHS** referring to the strata and the **RHS**
#'   referring to its label.
#'
#' @param level_labels A `<formula>` or list of formulas where each list-element
#'   is a formula with the **LHS** reflecting either the variable to re-label or
#'   a specific level, and the **RHS** reflecting what the new level should be
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
#' @param terms A `<formula>` or list of formulas selecting the model terms that
#'   should be used. The **LHS** is always the name of the variable that will be
#'   selected. The **RHS** is the potential label for the output table. If no label
#'   is desired, can place an `NA` value on right-side of formula.
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
