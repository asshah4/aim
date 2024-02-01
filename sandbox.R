# Sandbox ----
library(tidyverse)
library(survival)
library(gt)

# gt tables ----
clinical <- readRDS("../mims/_targets/objects/clinical")
models <- readRDS("../mims/_targets/objects/cox_models")
outcomes <- readRDS("../mims/_targets/objects/outcomes")
cox <- readRDS("../mims/_targets/objects/cox_compare")

data = extract_results(cox, how = "tidy", flat = TRUE, exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE)
terms = term ~ list(
	lf_stress = "HRV",
	lf_rest = "HRV",
	hf_stress = "HRV",
	hf_rest = "HRV",
	bpm_rest = "BPM",
	bpm_stress = "BPM",
	rdr_msi_bl1 = "MSIMI",
	lntroponin_rest = "Troponin"
)
models = name ~ list(
	death_base = "Unadjusted",
	death_msimi = "MSIMI",
	death_trop = "Troponin"
)
statistic = p.value ~ 0.05
values = c("estimate", "conf.low", "conf.high")
pattern = "{1} ({2}, {3})"
style = fill ~ list(color = "lightgreen")
decimals = 2
by = exposures ~ list(
	lf_stress = "Stress LF",
	lf_rest = "Rest LF",
	hf_stress = "Stress HF",
	hf_rest = "Rest HF",
	bpm_rest = "Rest Pulse",
	bpm_stress = "Stress Pulse"
)
missing_text = "."

tbl_compare(
	data = data,
	terms = terms,
	by = by,
	models = models,
	statistic = statistic,
	values = values,
	pattern = pattern,
	style = style,
	decimals = decimals,
	missing_text = missing_text
)


# Misc ----

level_labels = list(
  sex ~ c("Male", "Female"),
  black ~ c("White", "Black"),
  smoking ~ c(0, 1),
  alcohol ~ c(0, 1),
  obese ~ c("BMI < 30", "BMI >= 30"),
  osa ~ c("No", "Yes"),
  lvef_reduced ~ c("'LVEF < 40%'", "'LVEF >= 40%'")
)
