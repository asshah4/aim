# Setup
library(parsnip)
f <- mpg + cyl ~ wt + hp + gear
lm_mod <- linear_reg() %>% set_engine("lm")
b1 <- bullet(f, exposure = "wt", approach = "sequential", model = lm_mod)
b2 <- bullet(f, exposure = "gear", approach = "parallel", model = lm_mod)
a1 <- aim(list(b1 = b1))
a2 <- aim(list(b2 = b2))
sa <- specific_aims(a1, a2)

test_that("specific_aims() has correct inputs", {
	expect_s3_class(a1, "aim")
})

test_that("specific_aims() has correct outputs", {
	expect_s3_class(sa, "aims")
})
