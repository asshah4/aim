proj <-
	project() %>%
	set_data(mtcars) %>%
	add_hypothesis(
		name = "weight",
		formula = wt ~ vs,
		test = "t.test",
		paired = TRUE,
		combination = "direct"
	)

test_that("correct input", {
	expect_s3_class(proj, "project")
})

test_that("correct output", {
	expect_type(proj$hypothesis, "list")
})
