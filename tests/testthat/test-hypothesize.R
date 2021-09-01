test_that("hypothesis must be made with data present", {
	library(parsnip)
	expect_error({
		h <-
			hypothesize(
				h = mpg + hp ~ wt + cyl,
				combination = "sequential",
				test = linear_reg() %>% set_engine("lm")
			)
	})
	h <-
		hypothesize(
			h = mpg + hp ~ wt + cyl,
			combination = "sequential",
			test = linear_reg() %>% set_engine("lm"),
			data = mtcars
		)
	expect_type(attributes(h)$data, "list")
	expect_identical(attributes(h)$data, mtcars)
})

test_that("new hypotheses can easily be generated", {
	library(parsnip)
	h = mpg ~ X(wt) + hp + disp + cyl
	combination = "sequential"
	test = linear_reg() %>% set_engine("lm")
	h1 <- hypothesize(h, combination, test, data = mtcars)
	h2 <- update_hypothesis(h1, combination = "parallel")
	h3 <- update_hypothesis(h2, combination = "direct", data = iris)

	expect_s3_class(h2, "hypothesis")
	expect_length(attributes(h2)$parameters, 4)
	expect_identical(attributes(h3)$data, iris)
})

test_that("generic print methods work", {
	hyp <-
		hypothesize(
			h = mpg + hp ~ wt + cyl,
			combination = "sequential",
			test = linear_reg() %>% set_engine("lm"),
			data = mtcars
		)

	expect_output(print(hyp), regexp = "Hypothesis")

})
