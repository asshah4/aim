test_that("Sequential pattern expansion works appropriately", {

	# Simple fmls
	x <- fmls(witch ~ .x(wicked) + west + green, pattern = "sequential")
	y1 <- pattern_sequentially(x)
	expect_s3_class(y1, "fmls")
	expect_length(y1, 3)
	y2 <- pattern_sequentially(fmls(witch ~ .x(wicked) + west + green))
	y3 <- pattern_sequentially(witch ~ .x(wicked) + west + green)
	expect_message(pattern_sequentially(witch ~ .x(wicked) + west + green))
	expect_no_message(y2)
	expect_equal(y1, y2)
	expect_equal(y2, y3)

	# Simple formula
	x <- witch ~ wicked + west + green
	y1 <- pattern_sequentially(x)
	y2 <- pattern_sequentially(fmls(x))
	expect_length(y1, 3)
	expect_equal(y1, y2)

	# TODO mediation

	# TODO interaction

})

