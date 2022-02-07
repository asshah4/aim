test_that("purrr::possibly functions work", {
	m1 <- stats::lm(mpg ~ hp, mtcars)
	expect_s3_class(my_tidy(m1), "tbl_df")
	expect_s3_class(possible_tidy(m1), "tbl_df")
	expect_equal(possible_tidy("wrong class"), NA)

	library(parsnip)
	m2 <- possible_parsnip_fit(linear_reg() %>% set_engine("lm"), mpg ~ hp, mtcars)
	expect_s3_class(m2, "model_fit")

})
