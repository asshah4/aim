test_that("model tables can be initialized/created", {

	# New model table column requirements
	model_id = character()
	formula_id = numeric()
	data_id = character()
	model = character()
	formula = character()
	outcome = character()
	exposure = character()
	mediator = character()
	interaction = character()
	strata = character()
	level = factor()
	model_parameters = list()
	model_summary = list()
	fit_status = logical()
	formulaMatrix = data.frame()
	termTable = data.frame()

	# Empty builds
	x <- md_tbl()
	expect_equal(model_table(), x)
	expect_length(x, 14)


})

test_that("model can be initialized", {

	x <- fit(fmls(mpg ~ wt + hp + .s(am)), .fn = lm, data = mtcars, raw = FALSE)
	expect_length(x, 2)



})

