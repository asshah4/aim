test_that("inputs work", {

	f <- witch ~ wicked + west
	fl <- fmls(f)
	t <- tm(f)
	tl <- tmls(t)

	# Should be the same term list
	expect_equal(field(fl, "terms"), tl)
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

test_that("complex formulas can be appropriately split apart", {

	x <- fmls(witch + fairy ~ .x(wicked) + .x(good) + .m(west) + .m(east) + green)
	expect_length(x, 1)
	expect_equal(complexity(x), 4) # 2 x outcomes, 2 x exposures, 1 x mediator ...
	expect_length(simplify_outcomes(x), 2)
	expect_length(simplify_exposures(x), 2)
	expect_length(simplify_mediation(x), 6)
	# expect_length(simplify(x), 16) # Unique variables only

	# Evaluate `formula` objects instead
	x <- witch + fairy ~ wicked + good + west + east + green
	expect_equal(complexity(x), 4) # two outcomes
	expect_message(so <- simplify_outcomes(x))
	expect_message(sx <- simplify_exposures(x))
	expect_message(sm <- simplify_mediation(x))
	expect_length(so, 2)
	expect_length(sm, 1)
	expect_length(sx, 1)
	expect_message(s <- simplify(x))
	expect_length(s, 2)

	# Interaction terms can be simplified
	x1 <- witch ~ wicked * west + green
	x2 <- witch ~ wicked + west + wicked:west + green
	expect_message(x3 <- fmls(witch ~ .x(wicked) + .i(west) + green))
	expect_length(rhs(x1), 4)
	expect_equal(rhs(x1), rhs(x2))

})


