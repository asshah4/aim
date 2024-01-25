test_that("forest plot for interaction can be made", {
  
  expect_message({
    m <- 
      fmls(hp + mpg ~ .x(wt) + .i(am) + cyl, pattern = "sequential") |>
      fit(.fn = lm, data = mtcars, raw = FALSE)
  })
  object <- model_table(linear = m, data = mtcars)
  
  # Variables of interest for filtering are function arguments
  outcomes <- ""
})