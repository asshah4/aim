test_that("binary interaction models can generate joint CI", {

	# Survival data
	lung <- survival::lung
	object <-
		survival::coxph(survival::Surv(time, status) ~ wt.loss + sex + wt.loss:sex,
										data = lung)

	ints <-
		interaction_estimates(object,
													exposure = "wt.loss",
													binary = "sex",
													conf.level = 0.95)

	expect_length(ints, 5)
	expect_named(ints, c("estimate", "conf.low", "conf.high", "nobs", "level"))
})
