test_that("correct input", {
	expect_s3_class(project(), "project")
})

test_that("validation of data is correct", {
	expect_error({
		project() %>%
			set_data("xyz")
	})
})

test_that("correct print method applies for just datasets", {
	p <-
		project() %>%
		set_data(mtcars) %>%
		set_data(iris)

	expect_length(p, 4)
	expect_output(str(p), "List of 2")
})
