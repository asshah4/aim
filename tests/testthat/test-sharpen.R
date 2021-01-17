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
  	plan = am ~ mpg,
  	pattern = "parallel",
  	approach = logistic_reg() %>% set_engine("glm")
  ) %>%
	arm(
  	title = "vital_signs",
  	plan = vs ~ mpg,
  	pattern = "sequential",
  	approach = logistic_reg() %>% set_engine("glm")
  ) %>%
	equip()

sharp <- om %>% sharpen(which_arms = c("vital_signs", "automatic_transmission"))

test_that("sharpen() should return a additional column called metric", {
	expect_true(inherits(sharp$equipment, "list"))
	expect_output(str(sharp$equipment[[1]]), "metric")
})
