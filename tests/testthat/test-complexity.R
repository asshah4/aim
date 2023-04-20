test_that("formulas can be made with appropriate roles and complexity", {

	# Zeroeth order
	t <- tm(~x)
	expect_equal(complexity(t), 0)

	# First order
	t <- tm(y ~ x)
	expect_equal(complexity(t), 1)

	# Second order
	t <- tm(y ~ .x(x) + c)
	expect_equal(complexity(t), 2)

	# Third order/mediation
	t <- tm(y ~ .x(x) + .m(m) + c)
	expect_equal(complexity(t), 3)

	# Fourth order/script
	t <- tm(y1 + y2 ~ .x(x) + .m(m) + .c(c))
	expect_equal(complexity(t), 4)

})

test_that("mediation complexity can be determined", {

	# Simple mediation
	f <- Surv(stop, status) ~ .x(primary) + .x(secondary) + .m(mediator)
	t <- tm(f)
	expect_equal(complexity(f), 3)

	# Mediation with covariates
	f <- Surv(stop, status) + Surv(stop, censor) ~ .x(exposure) + .m(mediator) + confounder + covariate + predictor
	expect_equal(complexity(f), 4)

})


test_that("interaction complexity can be determined", {

	## Straightforward interaction terms
	# Would expect "formula" to be ... mpg ~ hp + gear + am + hp:am
	expect_message(t <- tm(mpg ~ .x(hp) + gear + .i(am)))
	expect_equal(complexity(t), 2)

	# No exposure
	expect_warning(t <- tm(mpg ~ gear + .i(am)))
	expect_equal(complexity(t), 2)

	# Order = 1
	expect_warning(t <- tm(mpg ~ .i(am)))
	expect_equal(complexity(t), 1)

	## Complex interactions (higher order)
	# Multiple exposures
	f <- wt + mpg ~ .x(hp) + cyl + .i(am)
	expect_message(t <- tm(f))
	expect_equal(complexity(t), 4) # For multiple outcomes


})

