library(magrittr)
library(parsnip)

# Pattern Testing -------------------------------------------------------------

# Direct pattern

test_that("arm() uses direct pattern correctly", {
	om <-
	  octomod() %>%
	  core(mtcars) %>%
	  arm(
	    title = "Horsepower",
	    plan = mpg + hp ~ wt + cyl,
	    pattern = "direct",
	    approach = "t.test",
	    paired = TRUE
	  )
	expect_s3_class(om$arms$Horsepower, "tbl")
	expect_true(inherits(om$arms, "list"))
})

# Parallel pattern
om <-
  octomod() %>%
  core(mtcars) %>%
  arm(
    title = "mileage",
    plan = mpg ~ hp + cyl + wt + disp + qsec,
    pattern = "parallel",
    approach = linear_reg() %>% set_engine("lm"),
    exposure = c("hp", "wt")
  )

test_that("arm() can use formula appropriately", {

  f <- om$arms$mileage$formulas[[1]]
	out <- unlist(strsplit(deparse(f[[2]]), "\ \\+\ "))
  om_out <- om$arms$mileage$outcomes[1]
  expect_equal(om_out, out)

})
