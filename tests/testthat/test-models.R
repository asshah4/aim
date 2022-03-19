test_that("models can be prescribed from base regressions", {

	m1 <- lm(mpg ~ hp + cyl, mtcars)
	m2 <- lm(mpg ~ hp + gear, mtcars)
	m3 <- glm(am ~ hp + cyl, mtcars, family = "binomial")
	m4 <- glm(am ~ hp + gear, mtcars, family = "binomial")
	m5 <- lm(hp ~ wt, mtcars)

	x1 <- model_rx(m1)
	x2 <- model_rx(m2)
	x3 <- model_rx(m3)
	x4 <- model_rx(m4)
	x5 <- model_rx(m5)

})
