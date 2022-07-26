test_that("ellipses can be hammered down to a single hierarchy list", {

	# Models
	m1 <- lm(mpg ~ hp + cyl, mtcars)
	m2 <- glm(am ~ hp + cyl, mtcars, family = "binomial")
	m3 <- lm(mpg ~ wt + gear, mtcars)
	m4 <- glm(am ~ wt + mpg, mtcars, family = "binomial")
	m5 <- md(m1)

	# Formulas
	f1 <- mpg ~ wt + cyl
	f2 <- fmls(f1)
	f3 <- fmls(mpg + wt ~ hp + cyl, order = 2)

	# Dot arguments
	object <- list(m1, second = m5, f = f1, f3)
	mc <- as.call(str2lang("list(m1, second = m5, f = f1, f3)"))
  arg_names <- as.character(mc)[-1]
  nms <- names(mc)[-1]
  nms[nms == ""] <- arg_names[nms == ""]
  name <- nms
  mtl <- hammer(object, name)
  expect_length(mtl, 5)
  expect_named(mtl[5], "f3_2")



})
