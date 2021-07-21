test_that("correct output", {
	proj <- project()
	expect_s3_class(proj, "project")
	expect_s3_class(proj, "list")
})
