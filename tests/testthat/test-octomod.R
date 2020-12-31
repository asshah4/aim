om <- octomod()
printed <- print.octomod(om)

test_that("octomod() creates appropriate list objects", {
	expect_length(om, 3)
	expect_true(inherits(om$core, "list"))
	expect_true(inherits(om$arms, "list"))
	expect_true(inherits(om$outfit, "list"))
})

test_that("octomod() is of appropriate class", {
	expect_s3_class(om, "octomod")
})

test_that("octomod() prints appropriately", {
	expect_output(str(om), "List of 3")
	expect_null(printed)
})
