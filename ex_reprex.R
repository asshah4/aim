#' ---
#' output: reprex::reprex_document
#' ---

library(vlndr)
x <- tm(witch ~ wicked)
ys <- apply_sequential_pattern(x)
