# Term vector implementation ----

test_that("`tm` objects can be generated and printed", {
	# Basic generation
	expect_length(new_tm(), 0)
	expect_s3_class(new_tm(), "tm")
	expect_output(print(new_tm()), "<term\\[0\\]>")
	expect_length(tm(), 0)
	expect_s3_class(tm(), "tm")
	expect_output(print(tm()), "<term\\[0\\]>")
	expect_equal(tm(), tm(tm()))
})

test_that("new `tm` can be made from character/atomic components", {

	ty <- tm(
		x = "Y",
		role = "outcome",
		label = "Dependent Variable",
		description = "Artificially created",
		distribution = "normal",
		type = "continuous"
	)

	tx <- tm(
		"X",
		role = "exposure",
		label = "Independent Variable",
		description = "Artificially created",
		distribution = "normal",
		type = "continuous"
	)

	tm <- tm(
		"M",
		role = "mediator",
		label = "Independent Variable",
		description = "Artificially created",
		distribution = "normal",
		type = "continuous"
	)

	tc <- tm(
		"C",
		role = "confounder",
		label = "Confounder",
		description = "Artificially created",
		distribution = "ordinal",
		type = "categorical"
	)

	tp <- tm(
		"P",
		role = "predictor",
		label = "Independent Variable",
		description = "Artificially created",
		distribution = "ordinal",
		type = "categorical"
	)

	ts <- tm(
		"S",
		role = "strata",
		label = "Stratification Variable",
		description = "Levels for data set",
		distribution = "binary",
		type = "categorical",
	)

	ti <- tm(
		"I",
		role = "interaction",
		label = "Interaction Variable",
		description = "Interaction for the exposure variable",
		distribution = "binary",
		type = "categorical",
	)

	t <- c(ty, tx, tm, tp, tc, ts, ti)

	expect_length(t, 7)
	expect_true(is_tm(t))
	expect_length(tm(character()), 0)

})

test_that("terms can be generated from a formula", {

	f1 <- output ~ input + modifier
	f2 <- output ~ .x(input) + modifier
	f3 <- output ~ .x(input) + log(modifier) + log(variable) + another
	expect_equal(lhs(f1), lhs(f2))
	expect_match(rhs(f2), ".x", all = FALSE)

	group = type = distribution = description = transformation = formula()
	rl <- input ~ "exposure"
	lb <- list(output ~ "The Final Outcome", input ~ "The First Mover")
	allArgs <-
		list(
			role = rl,
			label = lb,
			group = group,
			type = type,
			distribution = distribution,
			description = description,
			transformation = transformation
		)

	tms <- tm(f1, role = rl, label = lb)
	d <- vec_data(tms)
	expect_equal(d$role[d$term == "input"], "exposure")

	tms <- tm(f3, role = rl, label = lb)
	expect_s3_class(tms, "tm")
	expect_length(tms, 5)
	expect_equal(vec_data(tms)$label[1], "The Final Outcome")
	expect_length(tm(formula()), 0)

})

test_that("term coercion works", {

	f <- fmls(.o(wicked) ~ .x(witch) + west)
	t <- tm(f)
	expect_s3_class(t, "tm")

})

# Special terms ----

test_that("mediation terms will be assessed correctly", {

	x <- .o(output) ~ .x(input) + .m(mediator) + random
	t <- tm(x)
	expect_equal(field(t[3], "role"), "mediator")

})

test_that("term groups can be established", {

	x <- witch ~ glinda + wicked + west
	role = label = type = distribution = description = transformation = formula()
	group <- list(wicked ~ 1, west ~ 1)
	allArgs <-
		list(
			role = role,
			label = label,
			group = group,
			type = type,
			distribution = distribution,
			description = description,
			transformation = transformation
		)

	# Group by term implementation external to formula
	t <- tm(x, group = group)
	d <- vec_data(t)
	expect_equal(d$group[d$term == "wicked"], 1)

	### Group term implemntation using shortcuts

	# Expect .g to become .g0, and .g1 to be g1
	x1 <- witch ~ west + .g(green) + .g(wicked)
	x2 <- witch ~ west + .g(green) + .g1(wicked)
	t1 <- tm(x1)
	t2 <- tm(x2)
	expect_equal(describe(t1, "group")$green, describe(t2, "group")$green)
	expect_equal(describe(t2, "group")$wicked, 1)

	# Expect appropriate group levels
	x1 <- witch ~ west + .g1(green) + .g1(wicked)
	x2 <- witch ~ west + .g1(green) + .g2(wicked)
	t1 <- tm(x1)
	t2 <- tm(x2)
	expect_equal(describe(t1, "group")$green, describe(t2, "group")$green)
	expect_equal(describe(t2, "group")$wicked, 2)
})

test_that("interaction terms are appropriately made", {

	# From formula
	f <- witch ~ green + wicked*west
	expect_length(tm(f), 5)

	# Role-based interaction
	x <- witch ~ .x(wicked) + green + .i(west)
	role = label = group = type = distribution = description = transformation =
		formula()
	t <- tm(x)
	expect_message(tm(x))
	expect(length(t), 5)

})

# Term list implementation ----

test_that("term list wrappers can be generated", {

	expect_s3_class(new_tmls(), "tmls")
	expect_s3_class(tmls(), "tmls")
	expect_output(print(tmls()), "<term_list\\[0\\]>")
	expect_length(tmls(), 0)

	t1 <-
		tm(.o(good) ~ .x(bad) + ugly) |>
		tmls()
	expect_s3_class(t1, "tmls")
	expect_output(print(t1), "<term_list\\[1\\]>")

	x <- tm(.o(good) ~ .x(bad) + ugly)
	f <- .o(wicked) ~ .x(witch) + west
	y <- tm(f)
	expect_equal(tm(f), y)
	t2 <- tmls(x, y)
	t3 <- tmls(x, f)
	expect_s3_class(t2, "tmls")
	expect_length(t2, 2)
	expect_output(print(t2), "\\|")
	expect_equal(t2, t3)

})

# Term helpers ----

test_that("terms can be found and updated and attributes can be found", {

	object <- tm(output ~ input + .c(modifier))

	t <- components(object, role = "outcome")
	expect_length(t, 1)
	expect_equal(describe(t, "role")[[1]], "outcome")

	dots <- list(
		role = input ~ "exposure",
		label = list(output ~ "The Final Outcome", input ~ "The First Mover")
	)

	x <- update(
		object,
		role = input ~ "exposure",
		label = list(output ~ "The Final Outcome", input ~ "The First Mover")
	)

	y <- update(object, dots)

	expect_equal(x, y)
	expect_equal(vec_data(x)$role, c("outcome", "exposure", "confounder"))
})
