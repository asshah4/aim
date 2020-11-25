### AIM

# Setup testing
library(parsnip)
library(card)
data(geh)
f <- svg_mag + qrs_tang ~ lab_hba1c + age + sex + bmi + cad + htn
lm_mod <- parsnip::linear_reg() %>% parsnip::set_engine("lm")
b1 <- bullet(f, exposure = "lab_hba1c", approach = "sequential", model = lm_mod)
b2 <- bullet(f, exposure = "lab_hba1c", approach = "parallel", model = lm_mod)
bullets <- list(seq = b1, par = b2)
aims <- aim(bullets)

test_that("aim() creates a list of `aim` objects", {
	expect_equal(class(aims), "list")
})
