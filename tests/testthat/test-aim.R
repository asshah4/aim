### AIM

# Setup testing
library(parsnip)
f <- mpg + cyl ~ wt + hp + gear
lm_mod <- linear_reg() %>% set_engine("lm")
b <- bullet(f, exposure = "wt", approach = "sequential", model = lm_mod)
a <- aim(b)

test_that("aim() has the appropriate inputs and outputs", {
	expect_equal(class(b), "bullet")
	expect_s3_class(a, "aim")
})
