test_that("Sequential pattern expansion works appropriately", {

	# Simple fmls
	x <- fmls(witch ~ .x(wicked) + west + green, pattern = "sequential")
	y1 <- pattern_sequential(x)
	expect_s3_class(y1, "fmls")
	expect_length(y1, 3)
	y2 <- pattern_sequential(fmls(witch ~ .x(wicked) + west + green))
	y3 <- pattern_sequential(witch ~ .x(wicked) + west + green)
	expect_message(pattern_sequential(witch ~ .x(wicked) + west + green))
	expect_no_message(y2)
	expect_equal(y1, y2)
	expect_equal(y2, y3)

	# Simple formula
	x <- witch ~ wicked + west + green
	y1 <- pattern_sequential(x)
	y2 <- pattern_sequential(fmls(x))
	expect_length(y1, 3)
	expect_equal(y1, y2)

	# Mediation
	x <- witch ~ .x(wicked) + west + .m(green)
	yp <- pattern_sequential(x)
	expect_length(yp, 2)
	ys <- simplify(x)
	expect_length(ys, 3)
	yps <- simplify(yp)
	ysp <- pattern_sequential(ys)
	expect_length(yps, 4)
	expect_length(ysp, 4)

	# Interaction
	x <- witch ~ .x(wicked) + west + .i(green)
	y <- pattern_sequential(x)
	expect_length(y, 4)
	ys <- simplify(y)
	expect_equal(y, ys)
	expect_true(tm(y[4])[5] == "wicked:green")

})

test_that("parallel expansion works", {

	# Base formula
	x <- witch ~ wicked + west + green
	y <- pattern_parallel(x)
	expect_length(y, 3)

	# Interaction
	x <- fairy + witch ~ .x(wicked) + good + west + north + .i(green)
	y <- pattern_parallel(x)
	expect_length(y, 4)

	# Mediation
	x <- witch ~ .x(wicked) + west + north + .m(green)
	y <- pattern_parallel(x)
	expect_length(y, 2)

})

test_that("fundamental formulas can be extracted", {

	# Base formula to decompose
	x <- fairy + witch ~ wicked + good + west + north + green
	y <- unique(pattern_fundamental(x))
	expect_length(y, 10)

	# Complex special formula to be broken apart
	x <- fairy + witch ~ .x(wicked) + good + west + north + .i(green)
	y <- unique(pattern_fundamental(x))
	expect_length(y, 12)

})

test_that("direct formulas are appropriately expanded", {

	x <- fairy + witch ~ wicked + good + west + north + green
	y <- pattern_direct(x)
	expect_length(y, 1)

})

test_that("overall patterns can be expanded appropriately", {

	f1 <-
		fmls(fairy + witch ~ wicked + good + west + north + green,
				 pattern = "direct")
	expect_length(pattern(f1), 1)

	f2 <-
		fmls(fairy + witch ~ wicked + good + west + north + green,
				 pattern = "fundamental")
	expect_length(pattern(f2), 10)

	fm <-
		fmls(fairy + witch ~ .x(wicked) + good + west + north + .m(green),
				 pattern = "parallel")
	expect_length(pattern(fm), 3)

	fi <-
		fmls(fairy + witch ~ .x(wicked) + good + west + north + .i(green),
				 pattern = "sequential")
	expect_length(pattern(fi, 6))

})
