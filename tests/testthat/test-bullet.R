# Setup
library(parsnip)
f <- mpg + cyl ~ wt + hp + gear
lm_mod <- linear_reg() %>% set_engine("lm")
b <- bullet(f, exposure = "wt", approach = "sequential", model = lm_mod)

test_that("bullet() has correct inputs", {
	expect_s3_class(lm_mod, "model_spec")
})

test_that("bullet() function has correct outputs", {
	expect_s3_class(b, "bullet")
	expect_equal(length(class(b)), 1)
	expect_equal(length(b), 6)
})

