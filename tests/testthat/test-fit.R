test_that("lists can be fit", {

	f <- mpg + wt ~ X(hp) + X(cyl) + gear + drat + qsec
	labels <- list(mpg ~ "Mileage", hp ~  "Horsepower")
	tiers <- list(c(drat, qsec) ~ "speed", wt ~ "hardware")
	t <- tm(f, label = labels, tier = tiers)
	x <- rx(t, pattern = "sequential")
	lof <- fmls(x, order = 2)
	expect_s3_class(lof, "formula_archetype")

	lom <- fit(lof, lm, data = mtcars)
	expect_type(lom, "list")
	expect_length(lom, 12)
	expect_s3_class(lom[[1]], "lm")

	# Transform to model archetypes
	m1 <- model_archetype(lom)
	expect_s3_class(m1, "model_archetype")
	m2 <- fit(lof, lm, data = mtcars, archetype = TRUE)
	expect_equal(length(m1), length(m2))

	# Tidy
	tbl <- tidy(m1)
	expect_type(tbl, "list")
	expect_s3_class(tbl[[1]], "tbl_df")
	expect_length(tbl, 12)

	# Glance
	g <- glance(m1)
	expect_type(g, "list")
	expect_s3_class(g[[1]], "tbl_df")
	expect_length(g, 12)

})

test_that("fitting can be done with strata", {

	f <- mpg ~ X(wt) + hp + qsec + S(cyl) + S(am)
	labels <- list(mpg ~ "Mileage", hp ~  "Horsepower")
	tiers <- list(c(drat, qsec) ~ "speed", wt ~ "hardware")
	t <- tm(f, label = labels, tier = tiers)
	x <- rx(t, pattern = "sequential")
	lof <- fmls(x, order = 2)
	expect_s3_class(lof, "formula_archetype")

	# Transform to model archetypes
	ml <- fit(lof, lm, data = mtcars)
	ma <- fit(lof, lm, data = mtcars, archetype = TRUE)
	expect_type(ml, "list")
	expect_length(ml, 15)
	expect_s3_class(ml[[1]], "lm")
	m <- model_archetype(ml) # Expect to lose information here
	expect_s3_class(m, "model_archetype")
	# Because of how the model lists are made, the strata information will be lost
	# Has to stay within the "archetype" world to maintain that extra information
	# Thus, will not be the same metadata between generic mods and archetype mods
	expect_equal(vec_data(ma)[1], vec_data(m)[1], ignore_attr = TRUE)
	expect_equal(format(vec_data(ma)[[7]]), format(vec_data(m)[[7]]))
	expect_equal(vec_data(m)[["strata_info"]][1], NA_character_)

	# Tidy
	tbl <- tidy(m)
	expect_type(tbl, "list")
	expect_s3_class(tbl[[1]], "tbl_df")
	expect_length(tbl, 15)

	# Glance
	g <- glance(m)
	expect_type(g, "list")
	expect_s3_class(g[[1]], "tbl_df")
	expect_length(g, 15)


})

# Will skip this test
test_that("fit a survival model", {
	skip("Manual build of test only")

	# External data set
	df <- readRDS("../mims/_targets/objects/clinical")

	# Stratified analysis for survival model
	f1 <-
		rx(
			Surv(death_timeto, death_cv_yn) ~ X(hf_stress_rest_delta_zn) + hf_rest_ln_zn + age_bl + blackrace +  hx_hypertension_bl + hx_diabetes_bl + hx_hbchol_bl + cath_gensini_bl + ejection_fraction + S(female_bl),
			pattern = "sequential"
		) |>
		fmls(order = 2)

	f2 <-
		rx(
			Surv(death_timeto, death_cv_yn) ~ X(hf_stress_rest_delta_zn) + hf_rest_ln_zn + age_bl + female_bl +  hx_hypertension_bl + hx_diabetes_bl + hx_hbchol_bl + cath_gensini_bl + ejection_fraction + S(blackrace),
			pattern = "sequential"
		) |>
		fmls(order = 2)

	# Fitting them
	if (require(survival)) {
		sex <- fit(f1, .fit = coxph, data = df, archetype = TRUE)
		race <- fit(f2, .fit = coxph, data = df, archetype = TRUE)
	}
	# This setup allows it to keep the appropriate names
	m <- mdls(sex, race)
	expect_equal(length(sex), nrow(subset(m, grepl("sex", name))))
	expect_equal(length(race), nrow(subset(m, grepl("race", name))))

})
