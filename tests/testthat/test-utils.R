test_that("purrr::possibly functions work", {
	m1 <- stats::lm(mpg ~ hp, mtcars)
	expect_s3_class(my_tidy(m1), "tbl_df")
	expect_s3_class(possible_tidy(m1), "tbl_df")

})

test_that("table can be flattened", {

	library(survival) # Using lung data
	f <- Surv(time, status) ~ ph.karno + meal.cal + cluster(sex)
	object <- fmls(f, pattern = 'sequential')
	m <- fit(object, .fn = coxph, data = lung, raw = FALSE)
	x <- model_table(m)
	y <- flatten_table(x)

	expect_s3_class(flatten_table(x), "data.frame")
	expect_equal(min(y$number), 1)
	expect_equal(max(y$number), 3)
})
