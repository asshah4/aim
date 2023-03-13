test_that("inputs are appropriate", {

	f <- witch ~ wicked + west
	fl <- fmls(f)
	t <- tm(f)
	tl <- tmls(t)

	# Should be the same term list
	expect_equal(field(fl, "formula"), tl)
	expect_silent(validate_class(tl, c("formula", "fmls", "tmls", "tm")))

	expect_equal(complexity(f), complexity(fl))
	expect_equal(complexity(t), complexity(tl))
	expect_equal(complexity(fl), complexity(tl))
	expect_equal(complexity(t), 2)

})


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
	t <- tm(mpg ~ .x(hp) + gear + .i(am))
	expect_equal(complexity(t), 2)

	# No exposure
	t <- suppressWarnings(tm(mpg ~ gear + .i(am)))
	expect_equal(complexity(t), 2)

	# Order = 1
	t <- suppressWarnings(tm(mpg ~ .i(am)))
	expect_equal(complexity(t), 1)

	## Complex interactions (higher order)
	# Multiple exposures
	f <- mpg ~ .x(hp) + .x(cyl) + .i(am)
	t <- tm(f)
	expect_equal(complexity(t), 3) # For multiple exposures

	# Multiple interactions
	f <- mpg ~ .x(hp) + .i(am) + .i(vs)
	t <- tm(f)
	expect_equal(complexity(t), 2)

})

test_that("complex formulas can be appropriately split apart", {

	x <- fmls(witch + fairy ~ .x(wicked) + .x(good) + .m(west) + .m(east) + green)
	expect_length(x, 1)
	expect_equal(complexity(x), 4) # 2 x outcomes, 2 x exposures, 1 x mediator ...
	expect_length(simplify_outcomes(x), 2)
	expect_length(simplify_exposures(x), 2)
	expect_length(simplify_mediation(x), 6)
	expect_length(simplify(x), 16) # Unique variables only

	# Evaluate `formula` objects instead
	x <- witch + fairy ~ wicked + good + west + east + green
	expect_equal(complexity(x), 4) # two outcomes
	expect_length(simplify_outcomes(x), 2)
	expect_length(simplify_mediation(x), 1)
	expect_length(simplify_exposures(x), 1)
	expect_length(simplify(x), 2)


})
