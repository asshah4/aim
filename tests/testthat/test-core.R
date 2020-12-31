om1 <-
	octomod() %>%
	core(mtcars)

om2 <-
	om1 %>%
	change_core(iris)

om3 <-
	om2 %>%
	remove_core()

test_that("core() inherits a data.frame", {
	expect_true(inherits(om1$core, "data.frame"))
})

test_that("change_core() changes the data", {
	expect_identical(om2$core, iris)
})

test_that("remove_core() removes the data", {
	expect_length(om3$core, 0)
	expect_error(remove_core(om3))
})
