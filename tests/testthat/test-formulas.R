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

test_that("coercion by vctrs works", {

	# Characters
	f <- fmls(witch ~ wicked + west)
	expect_type(c(f, "test"), "character")
})
