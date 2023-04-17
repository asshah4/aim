test_that("fmls can be initialized and formatted", {

	# Empty class
	expect_length(fmls(), 0)

})

test_that("fmls-fmls can be combined", {

	# Vec ptype2 and cast testing

	f1 <- output ~ input + modifier
	f2 <- output ~ .x(input) + modifier
	f3 <- output ~ .x(input) + log(modifier) + log(variable) + another

	# Simple
	x <- fmls(f1)
	y <- fmls(f2)
	expect_s3_class(vec_ptype2(x, y), "fmls")

	# Should remove exposure variable from second term
	f <- vec_c(x, y)
	expect_length(f, 3)
	expect_equal(nrow(f), 2)
	expect_equal(f[1, ], f[2, ], ignore_attr = TRUE)
	expect_false("exposure" %in% vec_data(key_terms(f))$role)

	# Flipped version should enrich the exposure term value
	f <- vec_c(y, x)
	expect_equal(f[1, ], f[2, ], ignore_attr = TRUE)
	expect_true("exposure" %in% vec_data(key_terms(f))$role)
	expect_length(key_terms(f), 3)

	# More complex version
	x <- fmls(f1)
	y <- fmls(f3)
	f <- vec_c(x, y) # "weaker" version is first, richer is second

	expect_length(f, 6)
	expect_equal(nrow(f), 2)
	expect_s3_class(f, "fmls")
	expect_false("exposure" %in% vec_data(key_terms(f))$role)

	f <- vec_c(y, x) # Flip the order so the richer formula is first
	expect_equal(sum(f[1, ], na.rm = TRUE), 5)
	expect_equal(sum(f[2, ], na.rm = TRUE), 3)
	expect_s3_class(f, "fmls")
	expect_true("exposure" %in% vec_data(key_terms(f))$role)

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
	expect_type(vec_c(x, "test"), "character")
})


test_that("patterns can be included into formula", {

	f <- fmls(witch ~ wicked + west, pattern = "parallel")
	expect_equal(nrow(f), 2)

})

test_that("interaction terms can be included explicitly", {

	x <- witch ~ wicked + green + west + wicked:west
	expect_equal(as.character(fmls(x)), deparse1(x))

	expect_message(f <- fmls(witch ~ .x(wicked) + green + .i(west)))
	expect_equal(formula(f, env = .GlobalEnv)[[1]], x, ignore_attr = TRUE)

})

test_that("complex formulas/terms can be converted", {

	x <- tm(green + white ~ .x(wicked) + .x(good) + witch + fairy + magic + .m(west))
	f <- fmls(x)
	expect_equal(nrow(f), 10)

	f <- fmls(green ~ .x(wicked) + witch + west, pattern = "sequential")
	expect_equal(nrow(f), 3)

	f <- fmls(green ~ .x(wicked) + witch + west, pattern = "direct")
	expect_equal(nrow(f), 1)

	f <-
		fmls(witch ~ .x(wicked) + .x(good) + .m(magic) + west + north + green,
				 pattern = "sequential")
	expect_equal(nrow(f), 10)
})

