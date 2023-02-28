test_that("lst_fmls class generation works", {

	x <- .x(output) ~ input
	y <- apples + bananas ~ orange + (1 | peels)
	z <- ~ garbage_in
	f <- list(x, y, z)
	fl_1 <- lst_fmls(x, y, z)
	fl_2 <- list_of_formulas(f)
	expect_equal(fl_1, fl_2)
	expect_s3_class(fl_1, "lst_fmls")

	# Conversion
	expect_type(formula(fl_1), "list")

	# Explicit conversion to `lst_fmls`
	fl_3 <- lst_fmls(output ~ input, x ~ y)
	expect_length(fl_3, 2)
	expect_equal(lengths(fl_3), c(1, 1)) # Number of components in the list

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

