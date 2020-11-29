library(parsnip)
f <- mpg + cyl ~ wt + hp + gear
lm_mod <- linear_reg() %>% set_engine("lm")
b1 <- bullet(f, exposure = "wt", approach = "sequential", model = lm_mod)
b2 <- bullet(f, exposure = "gear", approach = "parallel", model = lm_mod)
bullets <- list(seq = b1, par = b2)
a <- aim(bullets)
out <- fire(a, data = mtcars)

test_that("fire() has appropriate inputs", {
	expect_s3_class(a, "aim")
})

test_that("fire() has appropriate outputs", {
	expect_s3_class(out, "aim")
	expect_true("fit" %in% names(out))
})
