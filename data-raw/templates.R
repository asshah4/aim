## code to prepare `templates` dataset goes here
.models <- c(
	'model_fit',
	'lm',
	'glm',
	'coxph',
	'crr',
	'tidycrr'
)

.patterns <- c(
	'fundamental',
	'direct',
	'sequential',
	'parallel',
	'rolling_interaction'
)

.transformations <- c(
	'log'
)

.roles <- list(
	'outcome' = '.o',
	'exposure' = '.x',
	'predictor' = '.p',
	'confounder' = '.c',
	'mediator' = '.m',
	'strata' = '.s',
	'interaction' = '.i'
)

usethis::use_data(.models,
									.patterns,
									.transformations,
									.roles,
									internal = TRUE,
									overwrite = TRUE)
