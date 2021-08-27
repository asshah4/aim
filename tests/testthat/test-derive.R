test_that("confounders can be found and manipulated", {
	library(parsnip)
	h <-
		hypothesize(
			mpg + hp ~ X(wt) + cyl,
			combination = "sequential",
			test = linear_reg() %>% set_engine("lm"),
			data = mtcars
		)

	x <-
		study() %>%
		draw(h) %>%
		construct() %>%
		derive("h") %>%
		construct() %>%
		extract("h_cut")

	expect_length(x, 11)

})

