om1 <-
	octomod() %>%
	add_core(mtcars)

om2 <-
	om1 %>%
	update_core(iris)

om3 <-
	om2 %>%
	remove_core()

test_that("add_core() inherits a data.frame", {
	expect_true(inherits(om1$core, "data.frame"))
})

test_that("update_core() changes the data", {
	expect_identical(om2$core, iris)
})

test_that("remove_core() removes the data", {
	expect_length(om3$core, 0)
})
