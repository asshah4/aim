#' @keywords internal
#' @noRd
has_cli <- function() {
  isTRUE(requireNamespace("cli", quietly = TRUE))
}
