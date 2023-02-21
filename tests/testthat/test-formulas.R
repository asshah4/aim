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

})
