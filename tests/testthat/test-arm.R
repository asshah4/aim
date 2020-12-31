library(magrittr)

om <-
  octomod() %>%
  core(mtcars) %>%
  arm(
    title = "Horsepower",
    f = disp ~ hp,
    pattern = "direct",
    approach = "t.test",
    paired = TRUE
  )

test_that("arm() creates the correct objects", {
	expect_s3_class(om$arms$Horsepower, "tbl")
	expect_true(inherits(om$arms, "list"))
})
