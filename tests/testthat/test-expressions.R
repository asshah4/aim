test_that("special formulas are expanded appropriately", {

	# Simple to complex formulas
	f1 <- mpg ~ wt
	f2 <- mpg ~ wt + cyl + gear

	# Check parallelization of predictors
	x <- expand_formula(f2, "parallel", table = TRUE)
	expect_equal(nrow(x), 3)

	# Check if fixed variables expanded correctly
	f3 <- mpg + hp ~ X(wt) + vs + gear
	f4 <- mpg ~ wt + vs + F(gear)
	f5 <- mpg ~ X(wt) + vs + F(gear)
	f6 <- mpg ~ X(wt) + X(qsec) + F((1 | cyl)) + vs + F(gear)

	x <- expand_formula(f4, "sequential", table = TRUE)
	expect_gt(length(x$vars[[2]]), length(x$vars[[1]]))

	y <- expand_formula(f5, "parallel", table = TRUE)
	expect_equal(nrow(y), 1)

	z <- expand_formula(f6, "sequential", table = TRUE)
	expect_equal(nrow(z), 4)
	expect_equal(length(z$vars[[1]]), length(z$vars[[3]]))

})
