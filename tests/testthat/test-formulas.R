test_that("`fmls` objects can be generated", {
	expect_length(new_fmls(), 0)
	expect_s3_class(new_fmls(), "fmls")
	expect_output(print(new_fmls()), "<formulas\\[0\\]>")
	expect_length(fmls(), 0)
	expect_s3_class(fmls(), "fmls")
	expect_output(print(fmls()), "<formulas\\[0\\]>")
})

test_that("simple formulas can be made from base::formula()", {

	f1 <- output ~ input + modifier
	f2 <- output ~ .x(input) + modifier
	f3 <- output ~ .x(input) + log(modifier) + log(variable) + another
	role = label = group = type = distribution = description = transformation = list()
	role <- input ~ "exposure"
	label <- list(output ~ "The Final Outcome", input ~ "The First Mover")

	x <- fmls(f1, role = role, label = label, group = group)
	y <- fmls(f2)
	z <- fmls(f3)
	f <- c(x, y, z)
	expect_s3_class(x, "fmls")
	expect_length(f, 3)

})

test_that("tm can convert to fmls objects", {

	t <- tm(.o(good) ~ .x(bad) + ugly)
	f <- fmls(t)
	fmls(t)
	expect_equal(f, fmls(t))

})

test_that("coercion by vctrs works", {

	# Characters
	x <- fmls(witch ~ wicked + west)
	expect_type(c(x, "test"), "character")
})


test_that("patterns can be included into formula", {

	f <- fmls(witch ~ wicked + west, pattern = "parallel")
	expect_equal(field(f, "pattern"), "parallel")

})

test_that("interaction terms can be included explicitly", {

	x <- witch ~ wicked + green + west + wicked:west
	expect_equal(as.character(fmls(x)), deparse1(x))

	f <- fmls(witch ~ .x(wicked) + green + .i(west))
	expect_equal(formula(f), x)

})
