test_that("the type of test is interpretted correctly", {

	library(parsnip)
	test_parsnip = linear_reg() %>% set_engine("lm")
	f <- mpg ~ wt + hp
	exposures <- "wt"

	h_parsnip <-
		hypothesize(
			f,
			exposures,
			combination = "sequential",
			test = test_parsnip,
			data = mtcars,
			strata = "am"
		)

	h_stat <-
		update_hypothesis(h_parsnip, test = stats::lm)

	x <-
		create_models() %>%
		add_hypothesis(h_parsnip) %>%
		add_hypothesis(h_stat) %>%
		construct_tests()

	fit_parsnip <- x$fit[x$level == 0 & x$number == 1 & x$name == "h_parsnip"][[1]]$fit
	fit_stat <- x$fit[x$level == 0 & x$number == 1 & x$name == "h_stat"][[1]]
	expect_equal(fit_parsnip$coefficients, fit_stat$coefficients)

})

test_that("htest will be translated appropriately", {

	# H-test examples:
		# t.test()
		# wilcox.test()
		# chisq.test()
		# cor.test()
		# fisher.test()
		# aov()
		# cov()
		# kruskal.test()

	# T.test using formula versus distribution
	h <- hypothesize(
		mpg ~ vs,
		test = stats::t.test,
		data = mtcars,
		combination = "direct"
	)

	x <-
		create_models() %>%
		add_hypothesis(h) %>%
		construct_tests()

	expect_s3_class(x$fit[[1]], "htest")

})
