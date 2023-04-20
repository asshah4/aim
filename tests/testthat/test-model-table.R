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

	# Empty builds should fail
	expect_error(md_tbl())
	expect_error(new_model_table())


})

test_that("model can be initialized", {

	x <- fit(fmls(mpg ~ wt + hp + .s(am)), .fn = lm, data = mtcars, raw = FALSE)
	expect_length(x, 2)

	m <- construct_model_table(x)
	expect_s3_class(m, "md_tbl")
	expect_length(m, 14)
	expect_equal(nrow(m), 2)

})

