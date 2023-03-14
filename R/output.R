#' @keywords internal
#' @noRd
has_cli <- function() {
  isTRUE(requireNamespace("cli", quietly = TRUE))
}


#' @keywords internal
#' @noRd
message_formula_to_fmls <- function() {
	message("Converting the supplied `formula` to a `fmls`")
}

