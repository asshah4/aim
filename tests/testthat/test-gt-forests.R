test_that("forest plot for interaction can be made", {

  f <- vec_c(fmls(hp + mpg ~ .x(wt) + .i(am) + cyl),
             fmls(hp + mpg ~ .x(wt) + .i(vs) + cyl)) |>
    suppressMessages()
  m <- fit(f, .fn = lm, data = mtcars, raw = FALSE)
  object <- model_table(linear = m, data = mtcars)

  # Variables of interest for filtering are function arguments
  outcomes <- list(hp ~ "Horsepower")
  exposures <- list(wt ~ "Weight")
  interactions <- list(am ~ "Transmission", vs ~ "Engine")


})
