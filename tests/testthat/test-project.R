proj <- project()

test_that("correct output", {
	expect_s3_class(proj, "project")
	expect_s3_class(proj, "tbl")
})
