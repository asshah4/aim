test_that("hypothesis can be added to study", {
	library(parsnip)
	h <-
		hypothesize(
			h = mpg + hp ~ wt + cyl,
			combination = "sequential",
			test = linear_reg() %>% set_engine("lm"),
			data = mtcars
		)

	f <- study() %>%
		add_hypothesis(h)

	expect_s3_class(f, "study")
})

test_that("studys can be fitted", {
	library(parsnip)
	hyp <- hypothesize(
			h = mpg + hp ~ wt + cyl,
			combination = "sequential",
			test = linear_reg() %>% set_engine("lm"),
			data = mtcars
		)
	f <- study() %>%
		add_hypothesis(hyp) %>%
		build_study()

	expect_type(f$fit, "list")
	expect_s3_class(f$tidy[[1]], "tbl_df")
})

test_that("multiple hypotheses can be added and fitted", {
	library(parsnip)
	h1 <- hypothesize(
		h = mpg ~ wt + cyl,
		combination = "sequential",
		test = linear_reg() %>% set_engine("lm"),
		data = mtcars
	)
	h2 <- update_hypothesis(h1, combination = "parallel")

	f <-
		study() %>%
		add_hypothesis(h1) %>%
		add_hypothesis(h2)

	# Should be a list of formulas with only a single data set saved
	expect_equal(nrow(f), 4)
	expect_equal(nrow(attr(f, "data_list")), 1)
	expect_equal(nrow(attr(f, "data_table")), 2)

	# Unadjusted h1 and h2 should be the same
	f <- f %>% build_study()
	t1 <- fetch_parameters(f, "h1")
	t2 <- fetch_parameters(f, "h2")
	expect_identical(t1[[1]], t2[[1]])
})

