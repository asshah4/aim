test_that("lst_fmls class generation works", {

	# Uses fmls as base object
	x1 <- .x(output) ~ input
	x2 <- apples + bananas ~ orange + (1 | peels)
	x3 <- ~ garbage_in
	f1 <- fmls(x1)
	f2 <- fmls(x2)
	f3 <- fmls(x3)
	fl <- c(f1, f2, f3)
	expect_length(lst_fmls(fl), 1)
	expect_length(lst_fmls(fl, f3), 2)

	# Explicit conversion to `lst_fmls`
	lof <- lst_fmls(output ~ input, x ~ y)
	expect_type(formula(lof), "list")
	expect_length(lof, 2)
	expect_equal(lengths(lof), c(1, 1)) # Number of components in the list

	# Test empty input
	expect_length(lst_fmls(), 0)
	x <- list()

	# General formula inputs
	a <- b <- c <- list()
	a <- input ~ "exposure"
	c <- list(output ~ "Out", input ~ "in")
	dots <- list(a = a, b = b, c = c)
	l1 <- lst_fmls(a = a)
	l2 <- lst_fmls(a)
	expect_equal(l1, l2, ignore_attr = TRUE)
	expect_length(lst_fmls(b), 0)
	expect_length(lst_fmls(b = b), 0)
	expect_error(lst_fmls(a = a, b = b, c = c))

})

test_that("lst_fmls will not lose attributes of terms", {

})


test_that("formulas are appropriately and safely placed into a list of formulas", {

	lof <- lst_fmls(x1, x2, x3)
	expect_length(lof, 3) # This are 3 different objects that are coerced to fmls
	expect_error(lst_fmls(list(x1, x2, x3)))
	expect_s3_class(lof, "lst_fmls")
})
