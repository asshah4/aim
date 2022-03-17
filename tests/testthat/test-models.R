test_that("models can be prescribed from base regressions", {

	m1 <- lm(mpg ~ hp + cyl, mtcars)
	m2 <- lm(mpg ~ hp + gear, mtcars)
	m3 <- glm(am ~ hp + cyl, mtcars, family = "binomial")
	m4 <- glm(am ~ hp + gear, mtcars, family = "binomial")
	m5 <- lm(hp ~ wt, mtcars)

})
