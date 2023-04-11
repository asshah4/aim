test_that("fmls can be initialized and formatted", {

	# Empty class
	expect_error(fmls())

})

test_that("fmls-fmls can be combined", {

	# Simple formulas
	f1 <- output ~ input + modifier
	f2 <- output ~ .x(input) + modifier
	f3 <- output ~ .x(input) + log(modifier) + log(variable) + another

	x <- fmls(f1)
	y <- fmls(f2)
	z <- fmls(f3)
	f <- c(x, y, z)
	expect_true(is_fmls(f))
	expect_s3_class(x, "fmls")
	expect_length(f, 3)

	# Print output
	expect_output(print(format(x)), "output|input|modifier")


})

test_that("tm can convert to fmls objects", {

	t <- tm(.o(good) ~ .x(bad) + ugly)
	f <- fmls(t)
	expect_equal(f, fmls(t))

})

test_that("fmls can be coerced to character class", {

	# Characters
	x <- fmls(witch ~ wicked + west)
	expect_type(c(x, "test"), "character")
})


test_that("patterns can be included into formula", {

	f <- fmls(witch ~ wicked + west, pattern = "parallel")
	expect_length(f, 2)

})

test_that("interaction terms can be included explicitly", {

	x <- witch ~ wicked + green + west + wicked:west
	expect_equal(as.character(fmls(x)), deparse1(x))

	expect_message(f <- fmls(witch ~ .x(wicked) + green + .i(west)))
	expect_equal(formula(f, env = .GlobalEnv), x, ignore_attr = TRUE)

})

test_that("complex formulas/terms can be converted", {

	x <- tm(green + white ~ .x(wicked) + .x(good) + witch + fairy + magic + .m(west))
	f <- fmls(x)
	expect_length(f, 10)

	f <- fmls(green ~ .x(wicked) + witch + west, pattern = "sequential")
	expect_length(f, 3)

	f <- fmls(green ~ .x(wicked) + witch + west, pattern = "direct")
	expect_length(f, 1)

	x <-
		fmls(witch ~ .x(wicked) + .x(good) + .m(magic) + west + north + green,
				 pattern = "sequential")
	expect_length(x, 10)
})
