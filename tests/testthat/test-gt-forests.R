test_that("forest plot for interaction can be made", {

  expect_message({
    m <-
      fmls(hp + mpg ~ .x(wt) + .i(am) + .i(vs), pattern = "parallel") |>
      fit(.fn = lm, data = mtcars, raw = FALSE)
  })
  object <- model_table(linear = m, data = mtcars)

  # Variables of interest for filtering are function arguments
  outcomes <- list(hp ~ "Horsepower")
  exposures <- list(wt ~ "Weight")
  interactions <- list(am ~ "Transmission", vs ~ "Engine")


  vlndr::fmls(hp ~ .x(wt) + .i(am), pattern = 'parallel')
})

library(vlndr)
fmls(hp ~ .x(wt) + .i(am), pattern = 'parallel')
