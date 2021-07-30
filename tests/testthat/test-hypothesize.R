test_that("can add data to hypothesis", {
	library(parsnip)
	expect_warning({
		h <-
			hypothesize(
				h = mpg + hp ~ wt + cyl,
				combination = "sequential",
				test = linear_reg() %>% set_engine("lm")
			)
	})
	h <- add_data(h, mtcars)
	expect_type(attributes(h)$data, "list")
	expect_identical(attributes(h)$data$mtcars, mtcars)
})
