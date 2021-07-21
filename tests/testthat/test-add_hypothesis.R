library(parsnip)

test_that("formulas will make appropriate tables", {
	# Using mtcars as test data
	proj <-
		project() %>%
		set_data(mtcars)

	# Sequential = 4 rows
	p <-
		proj %>%
		add_hypothesis(
			name = "test",
			formula = mpg ~ vs + am + wt + hp,
			combination = "sequential",
			test = linear_reg() %>% set_engine("lm")
		)

	expect_equal(nrow(p$hypothesis$test), 4)

	# Direct = 1 row x number of exposures
	p <-
		proj %>%
		add_hypothesis(
			name = "test",
			formula = mpg ~ exp(vs) + exp(am) + wt + hp,
			combination = "direct",
			test = linear_reg() %>% set_engine("lm")
		)

	expect_equal(nrow(p$hypothesis$test), 2)

	# Parallel = number of predictors minus number of fixed covariates
	p <-
		proj %>%
		add_hypothesis(
			name = "test",
			formula = mpg ~ cov(vs) + am + wt + hp,
			combination = "parallel",
			test = linear_reg() %>% set_engine("lm")
		)

	expect_equal(nrow(p$hypothesis$test), 3)

	# Exposures that contain interaction terms will be appropriately expanded
	p <-
		proj %>%
		add_hypothesis(
			name = "test",
			formula = mpg ~ exp(vs*am) + wt + hp,
			combination = "sequential",
			test = linear_reg() %>% set_engine("lm")
		)

	expect_true(any(grepl("\\:", p$hypothesis$test$formulas[[1]])))
})


test_that("different formula modifiers lead to correct number of formulas generated", {
	# Using mtcars as test data
	proj <-
		project() %>%
		set_data(mtcars)

	# Sequential
	p <-
		proj %>%
		add_hypothesis(
			name = "test1",
			formula = mpg ~ vs + am,
			combination = "sequential",
			test = linear_reg() %>% set_engine("lm")
		) %>%
		add_hypothesis(
			name = "test2",
			formula = mpg ~ exp(vs) + am + wt,
			combination = "sequential",
			test = linear_reg() %>% set_engine("lm")
		) %>%
		add_hypothesis(
			name = "test3",
			formula = mpg ~ cov(vs) + am + wt,
			combination = "sequential",
			test = linear_reg() %>% set_engine("lm")
		) %>%
		add_hypothesis(
			name = "test4",
			formula = mpg ~ cov(vs) + cov(am) + wt + gear,
			combination = "sequential",
			test = linear_reg() %>% set_engine("lm")
		) %>%
		add_hypothesis(
			name = "test5",
			formula = mpg ~ exp(vs) + exp(am) + wt + gear,
			combination = "sequential",
			test = linear_reg() %>% set_engine("lm")
		) %>%
		add_hypothesis(
			name = "test6",
			formula = mpg ~ exp(vs) + exp(am) + cov(wt) + cov(gear),
			combination = "sequential",
			test = linear_reg() %>% set_engine("lm")
		)

	expect_length(p$hypothesis$test1$formulas, 2)
	expect_length(p$hypothesis$test2$formulas, 3)
	expect_length(p$hypothesis$test3$formulas, 2)
	expect_length(p$hypothesis$test4$formulas, 2)
	expect_length(p$hypothesis$test5$formulas, 6)
	expect_length(p$hypothesis$test6$formulas, 2)

	# Parallel
	p <-
		proj %>%
		add_hypothesis(
			name = "test1",
			formula = mpg ~ vs + am,
			combination = "parallel",
			test = linear_reg() %>% set_engine("lm")
		) %>%
		add_hypothesis(
			name = "test2",
			formula = mpg ~ exp(vs) + am + wt,
			combination = "parallel",
			test = linear_reg() %>% set_engine("lm")
		) %>%
		add_hypothesis(
			name = "test3",
			formula = mpg ~ cov(vs) + am + wt,
			combination = "parallel",
			test = linear_reg() %>% set_engine("lm")
		) %>%
		add_hypothesis(
			name = "test4",
			formula = mpg ~ cov(vs) + cov(am) + wt + gear,
			combination = "parallel",
			test = linear_reg() %>% set_engine("lm")
		) %>%
		add_hypothesis(
			name = "test5",
			formula = mpg ~ exp(vs) + exp(am) + wt + gear,
			combination = "parallel",
			test = linear_reg() %>% set_engine("lm")
		) %>%
		add_hypothesis(
			name = "test6",
			formula = mpg ~ exp(vs) + exp(am) + cov(wt) + cov(gear),
			combination = "parallel",
			test = linear_reg() %>% set_engine("lm")
		)

	expect_length(p$hypothesis$test1$formulas, 2)
	expect_length(p$hypothesis$test2$formulas, 2)
	expect_length(p$hypothesis$test3$formulas, 2)
	expect_length(p$hypothesis$test4$formulas, 2)
	expect_length(p$hypothesis$test5$formulas, 4)
	expect_length(p$hypothesis$test6$formulas, 2)

})
