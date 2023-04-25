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

test_that("model constructors work for initialization", {

	x <- fit(fmls(mpg ~ wt + hp + .s(am)), .fn = lm, data = mtcars, raw = FALSE)
	expect_length(x, 2)

	m <- construct_model_table(x)
	expect_s3_class(m, "md_tbl")
	expect_length(m, 14)
	expect_equal(nrow(m), 2)

})

test_that("model table inputs can be parsed and incorporated", {

	x <-
		fit(fmls(mpg ~ wt + hp + .s(am)),
				.fn = lm,
				data = mtcars,
				raw = FALSE)
	y <-
		fit(
			fmls(am ~ disp + cyl),
			.fn = glm,
			family = "binomial",
			data = mtcars,
			raw = FALSE
		)

	dots <- list(x, y)

	z <- model_table(dots)
	expect_s3_class(z, "md_tbl")
	expect_output(print(z), "<md_tbl>")
	expect_equal(nrow(z), 3)
	expect_length(z, 14)
	expect_length(attr(z, "termTable")$term, 7)
	expect_length(unique(attr(z, "termTable")$term), 6)
	expect_length(attr(z, "formulaMatrix"), 6)

})

