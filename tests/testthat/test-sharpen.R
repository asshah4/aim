library(magrittr)
library(parsnip)

df <- mtcars
df$am <- factor(df$am)
df$vs <- factor(df$vs)

om <-
	octomod() %>%
	core(df) %>%
  arm(
  	title = "automatic_transmission",
  	f = am ~ mpg,
  	pattern = "parallel",
  	approach = logistic_reg() %>% set_engine("glm")
  ) %>%
	arm(
  	title = "vital_signs",
  	f = vs ~ mpg,
  	pattern = "sequential",
  	approach = logistic_reg() %>% set_engine("glm")
  ) %>%
	equip()

sharp <- om %>% sharpen(which_arms = c("vital_signs", "automatic_transmission"))

test_that("sharpen() should return a additional column called metric", {
	expect_true(inherits(sharp$outfit, "list"))
	expect_output(str(sharp$outfit[[1]]), "metric")
})
