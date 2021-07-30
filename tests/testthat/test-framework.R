test_that("data can be added to a framework", {
	library(parsnip)
	h <-
		hypothesize(
			h = mpg + hp ~ wt + cyl,
			combination = "sequential",
			test = linear_reg() %>% set_engine("lm")
		)

	f <- framework() %>%
		add_hypothesis(h)

	expect_s3_class(f, "framework")

	# Still erroring
	f %>%
		add_data(f, "h", mtcars)

	expect_type(attributes(h)$data, "list")
	expect_identical(attributes(h)$data$mtcars, mtcars)
})
