test_that("findings are in table form", {
	proj <-
		project() %>%
		set_data(mtcars) %>%
		make_hypothesis(
			name = "weight",
			formula = wt ~ vs,
			test = "t.test",
			paired = TRUE,
			combination = "direct"
		) %>%
		build_models()

	res <- proj %>% collect_findings()
	expect_s3_class(res, "tbl")
})
