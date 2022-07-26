## code to prepare `supported_models` dataset goes here
supported_models <- c(
	"model_fit",
	"lm",
	"glm",
	"coxph"
)

usethis::use_data(supported_models, internal = TRUE, overwrite = TRUE)
