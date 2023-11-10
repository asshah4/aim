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
