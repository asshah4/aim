test_that("for dichotomous variables", {

	object <-
		targets::tar_read(cox_mdls, store = '../mims/_targets') |>
		dplyr::filter(name == 'parsimonious')

	data <-
		targets::tar_read(clinical, store = '../mims/_targets') |>
		dplyr::mutate(lf_delta_bin = factor(
			lf_delta_bin,
			levels = c(1, 0),
			labels = c('Yes', 'No')
		)) |>
		dplyr::mutate(lf_rest_quartile = factor(
			lf_rest_quartile,
			levels = c(1, 0),
			labels = c('Yes', 'No')
		))

	outcomes <-
		list('death_cv_yn' ~ 'Cardiovascular mortality',
				 'death_any_yn' ~ 'All-cause mortality')

	followup <- 'death_timeto'

	terms <- list(lf_delta_bin ~ 'Mental stress-induced HRV decrease',
								lf_rest_quartile ~ 'Low rest HRV')

	adjustment <-
		list(
			3 ~ 'Rate per 100 person-years',
			5 ~ 'Adjusted for demo',
			7 ~ 'Adjust for above + clinical',
			8 ~ 'Adjust for above + stress testing'
		)

	rate_difference <- TRUE

	gtbl <- tbl_dichotomous_hazard(
		object = object,
		data = data,
		outcomes = outcomes,
		follow = followup,
		terms = terms,
		adjustment = adjustment,
		rate_difference = rate_difference
	)

	expect_s3_class(gtbl, 'gt_tbl')

})

test_that("for categorical variables", {

	object <-
		targets::tar_read(cox_mdls, store = '../mims/_targets') |>
		dplyr::filter(name == 'groups')

	data <-
		targets::tar_read(clinical, store = '../mims/_targets') |>
		dplyr::mutate(lf_grps = factor(
			lf_grps,
			levels = c(0, 1, 2, 3),
			labels = c(
				'Normal rest & stress-induced increase',
				'Low rest & stress-induced increase',
				'Normal rest & stress-induced decrease',
				'Low rest & stress-induced decrease'
			)
		)) |>
		dplyr::mutate(hf_grps = factor(
			hf_grps,
			levels = c(0, 1, 2, 3),
			labels = c(
				'Normal rest & stress-induced increase',
				'Low rest & stress-induced increase',
				'Normal rest & stress-induced decrease',
				'Low rest & stress-induced decrease'
			)
		))

	outcomes <-
		list(death_cv_yn ~ 'Cardiovascular mortality',
				 death_any_yn ~ 'All-cause mortality')

	followup <- 'death_timeto'

	terms <- lf_grps ~ 'HRV response category v. reference'

	adjustment <- list(2 ~ 'Rate per 100 person-years',
										 5 ~ 'Adjusted for demo',
										 7 ~ 'Adjust for above + clinical',
										 8 ~ 'Adjust for above + stress testing')

	rate_difference <- FALSE

	gtbl <- tbl_categorical_hazard(
		object = object,
		data = data,
		outcomes = outcomes,
		follow = followup,
		terms = terms,
		adjustment = adjustment,
		rate_difference = rate_difference
	)

	expect_s3_class(gtbl, 'gt_tbl')
})
