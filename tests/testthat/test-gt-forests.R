test_that("forest plot for interaction can be made", {
  
  skip()

  f <- vec_c(fmls(hp + mpg ~ .x(wt) + .i(am) + cyl),
             fmls(hp + mpg ~ .x(wt) + .i(vs) + cyl)) |>
    suppressMessages()
  m <- fit(f, .fn = lm, data = mtcars, raw = FALSE)
  object <- model_table(linear = m, data = mtcars)

  # Variables of interest for filtering are function arguments
  outcomes <- list(hp ~ "Horsepower")
  exposures <- list(wt ~ "Weight")
  interactions <- list(am ~ "Transmission", vs ~ "Engine")
  level_labels <- list(am ~ c("Automatic", "Manual"),
                     vs ~ c("V8", "V6"))

  # Forest plot modifying variables
  columns <- list(beta ~ "Estimate", conf ~ "95% CI", n ~ "No.", p ~ "Interaction p-value")
  axis <- list(scale ~ "continuous", title ~ "Forest")
  width <- list(forest ~ 0.4)
  forest <- list()

  gtbl <-
    tbl_interaction_forest(
      object = object,
      outcomes = outcomes,
      exposures = exposures,
      interactions = interactions,
      level_labels = level_labels,
      columns = columns,
      axis = axis,
      width = width,
      forest = forest
    )

  expect_s3_class(gtbl, "gt_tbl")
  expect_contains(unlist(gtbl$`_boxhead`$column_label), c("Estimate", "Forest"))
  expect_equal(nrow(gtbl$`_stub_df`), 5)
  expect_equal({
    dat <- gtbl$`_styles`
    # Second row should be colored white for p-value
    sty <- dat[dat$colname == "p_value" & dat$rownum == 2, ]$styles[[1]]
    sty$cell_text$color
  }, "#FFFFFF")
  expect_equal({
    dat <- gtbl$`_styles`
    # First row should be vertically aligned at the bottom
    sty <- dat[dat$colname == "p_value" & dat$rownum == 1, ]$styles[[1]]
    sty$cell_text$v_align
  }, "bottom")

})
