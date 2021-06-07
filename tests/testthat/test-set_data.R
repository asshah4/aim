proj <-
	project() %>%
	set_data(mtcars)

test_that("correct input", {
	expect_s3_class(proj, "project")
})

test_that("correct output", {
	expect_length(proj$title, 1)
	expect_s3_class(proj$data$mtcars, "data.frame")
})

