#' ---
#' output: reprex::reprex_document
#' ---

library(vlndr)
fmls(hp ~ .x(wt) + .i(am), pattern = "parallel")
