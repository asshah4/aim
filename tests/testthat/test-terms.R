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

	tms <- tm(f3, role = rl, label = lb)
	expect_s3_class(tms, "tm")
	expect_length(tms, 5)
	expect_equal(vec_data(tms)$label[1], "The Final Outcome")

})
