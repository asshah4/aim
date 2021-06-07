proj <-
	project() %>%
	set_data(mtcars) %>%
	add_hypothesis(
		name = "weight",
		formula = wt ~ vs,
		test = "t.test",
		paired = TRUE,
		combination = "direct"
	) %>%
	build_models()

test_that("correct input", {
	expect_s3_class(proj, "project")
})

test_that("correct output", {
	res <- unlist(proj$findings, recursive = FALSE)
	expect_type(proj$findings, "list")
	expect_length(res, 1)
	res <- res$weight
	expect_true("tidied" %in% names(res))
})
