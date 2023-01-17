test_that("`fmls` objects can be generated", {
	expect_length(new_fmls(), 0)
	expect_s3_class(new_fmls(), "fmls")
	expect_output(print(new_fmls()), "<formulas\\[0\\]>")
	expect_length(fmls(), 0)
	expect_s3_class(fmls(), "fmls")
	expect_output(print(fmls()), "<formulas\\[0\\]>")
})
