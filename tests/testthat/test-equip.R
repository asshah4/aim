om1 <- octomod()
om2 <-
	om1 %>%
  add_core(iris) %>%
  add_arm(
    title = "t_test",
    f = Sepal.Length + Sepal.Width ~ Petal.Length,
    pattern = "direct",
    approach = "t.test",
    paired = TRUE
  )
om3 <-
	om2 %>%
	equip()

test_that("add_outfit() should error if octomod isn't ready", {
	expect_error(
		om1 %>%
			equip()
	)
	expect_error(
		om1 %>%
			add_core(iris) %>%
			equip()
	)
})

test_that("add_outfi() should have correct output", {
	expect_true(inherits(om3$outfit, "list"))
	expect_true(inherits(om3$outfit[[1]], "tbl_df"))
	expect_true(inherits(om3$outfit[[1]]$tidied, "list"))
	expect_true(inherits(om3$outfit[[1]]$tidied[[1]], "tbl_df"))
})
