test_that("ellipses can be hammered down to a single hierarchy list", {

	# Models
	m1 <- lm(mpg ~ hp + cyl, mtcars)
	m2 <- glm(am ~ hp + cyl, mtcars, family = "binomial")
	m3 <- lm(mpg ~ wt + gear, mtcars)
	m4 <- glm(am ~ wt + mpg, mtcars, family = "binomial")
	m5 <- mx(m1)

	# Formulas
	f1 <- mpg ~ wt + cyl
	f2 <- fmls(f1)
	f3 <- fmls(mpg + wt ~ hp + cyl, order = 2)

	# Dot arguments
	args <- list(m5, second = m2, f = f1, f3)
	mc <- as.call(str2lang("list(m5, second = m2, f = f1, f3)"))
  arg_names <- as.character(mc)[-1]
  nms <- names(mc)[-1]
  nms[nms == ""] <- arg_names[nms == ""]
  name <- nms
	mtl <- hammer(m5, second = m2, f = f1, f3)
  expect_length(mtl, 5)
  expect_named(mtl[5], "f3_2")

})

test_that("forged objects can be shaped into a informative table", {

	m1 <- lm(mpg ~ hp + cyl, mtcars)
	m2 <- glm(am ~ hp + cyl, mtcars, family = "binomial")
	m3 <- lm(mpg ~ wt + gear, mtcars)
	m4 <- glm(am ~ wt + mpg, mtcars, family = "binomial")
	m <- list(m1, m2, m3, m4)
	object <-
		mx(m) |>
		forge()

	# Tempering it to remove nested data frames (par_est and mod_inf)
	x <- temper(object)
	expect_s3_class(x, "tbl_df")
	expect_named(x)
	expect_length(x, 33) # Number of currently columns

})

