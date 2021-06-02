test_that("can expect a `project` as main input", {
	proj <- project()

	expect_s3_class(proj, "project")
})
