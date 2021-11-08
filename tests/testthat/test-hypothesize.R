test_that("labeled formulas are expanded appropriately", {

	# Simple to complex formulas
	f1 <- mpg ~ wt
	f2 <- mpg ~ wt + cyl + gear

	# Check parallelization of predictors
	x <- expand_formula(f2, combination = "parallel", table = TRUE)
	expect_equal(nrow(x), 3)

	# Check if fixed variables expanded correctly
	rx::set_rx_theme("murmur")
	f3 <- rx::rx(mpg + hp ~ X(wt) + vs + gear)
	f4 <- rx::rx(mpg ~ wt + vs + F(gear))
	f5 <- mpg ~ wt + vs + gear

	x <- expand_formula(f4, labels = labels(f4), combination = "sequential", table = TRUE)
	expect_gt(length(x$vars[[2]]), length(x$vars[[1]]))

	y <- expand_formula(
		f5,
		labels = list(exposures = "wt", fixed = "gear"),
		combination = "parallel",
		table = TRUE
	)
	expect_equal(nrow(y), 1)

})

test_that("hypothesis validation of arguments is appropriate", {
	library(parsnip)

	# missing data
	expect_error({
		h <-
			hypothesize(
				x = mpg + hp ~ wt + cyl,
				combination = "sequential",
				test = linear_reg() %>% set_engine("lm")
			)
	})

	# Appropriate attributes
	h <-
		hypothesize(
			x = mpg + hp ~ wt + cyl,
			exposures = "wt",
			combination = "sequential",
			test = linear_reg() %>% set_engine("lm"),
			data = mtcars
		)
	expect_type(attributes(h)$data, "list")
	expect_identical(attributes(h)$data, mtcars)
})

test_that("hypotheses can easily be generated", {

	# Updating functions
	library(parsnip)
	f <- mpg ~ wt + hp + disp + cyl + vs
	combination <- "sequential"
	test <- linear_reg() %>% set_engine("lm")
	h1 <- hypothesize(
		f, exposures = "wt",
		combination = combination,
		test = test,
		data = mtcars
	)
	h2 <- update_hypothesis(h1, combination = "parallel")
	h3 <- update_hypothesis(h2, data = mtcars, strata = "vs")

	expect_s3_class(h2, "hypothesis")
	expect_false("vs" %in% labels(stats::terms(h3)))
	expect_true("vs" %in% labels(stats::terms(h2)))

	# Check printing function
	expect_output(print(h1), regexp = "Hypothesis")
})

test_that("accepted objects are errored appropriately", {

	# List should fail
	expect_error({
		hypothesize(
			x = list(outcomes = "mpg", exposures = "wt", confounders = "cyl"),
			combination = "direct",
			test = linear_reg() %>% set_engine("lm"),
			data = mtcars
		)
	})

	# Dagitty and tidy dagitty should fail
	dag <- ggdag::dagify(mpg ~ wt + cyl)
	expect_error({
		hypothesize(
			x = dag,
			combination = "direct",
			test = linear_reg() %>% set_engine("lm"),
			data = mtcars
		)
	})
	expect_error({
		hypothesize(
			x = ggdag::tidy_dagitty(dag),
			combination = "direct",
			test = linear_reg() %>% set_engine("lm"),
			data = mtcars
		)
	})

})

test_that("formula and rx classes work appropriately", {

	# Labels for formulas
	library(parsnip)
	f <- mpg ~ wt + hp + disp + cyl + vs
	exposures <- c("wt", "hp")
	combination <- "sequential"
	test = linear_reg() %>% set_engine("lm")
	h1 <- hypothesize(
		f, exposures = exposures,
		combination = combination,
		test = test,
		data = mtcars
	)

	# Rx system
	library(rx)
	rx::set_rx_theme("murmur")
	f <- rx::rx(mpg ~ X(wt) + X(hp) + disp + cyl + vs)
	combination <- "sequential"
	test = linear_reg() %>% set_engine("lm")
	h2 <- hypothesize(f, combination, test, data = mtcars)

	expect_equal(h1, h2, ignore_attr = TRUE)

	rx::reset_rx_roles()

})
