test_that("hypothesis can be added to framework", {
	library(parsnip)
	h <-
		hypothesize(
			h = mpg + hp ~ wt + cyl,
			combination = "sequential",
			test = linear_reg() %>% set_engine("lm"),
			data = mtcars
		)

	f <- framework() %>%
		add_hypothesis(h)

	expect_s3_class(f, "framework")

})

test_that("data added later is same as data added in original hypothesis", {
	hyp <- hypothesize(
			h = mpg + hp ~ wt + cyl,
			combination = "sequential",
			test = linear_reg() %>% set_engine("lm")
		)

	f1 <- framework() %>%
		add_hypothesis(hyp) %>%
		add_data("hyp", mtcars)

	hyp <- hypothesize(
			h = mpg + hp ~ wt + cyl,
			combination = "sequential",
			test = linear_reg() %>% set_engine("lm"),
			.data = mtcars
		)

	f2 <- framework() %>%
		add_hypothesis(hyp)

	expect_equal(f1, f2, ignore_attr = TRUE)
})

test_that("frameworks can be fitted", {
	library(parsnip)
	hyp <- hypothesize(
			h = mpg + hp ~ wt + cyl,
			combination = "sequential",
			test = linear_reg() %>% set_engine("lm"),
			.data = mtcars
		)
	f <- framework() %>%
		add_hypothesis(hyp) %>%
		build_frames()

	expect_type(f$fit, "list")
	expect_s3_class(f$tidy[[1]], "tbl_df")
})
