#' Equip the `octomod` With Hypothesis Testing
#'
#' This function allows for fitting the many arms of the `octomod` object,
#' equipping them with the correct test methods. For modeling, this heavily
#' leans on the `tidymodels` approach which allows for unified model
#' specification. For inferential statistics, which do not yet have a unified
#' testing approach, this provides the ability to run these somewhat simpler
#' tests in a cohesive manner.
#'
#' @return An `octomod` object with arms equipped with the test statistics.
#'
#' @param octomod Object of class `octomod`
#'
#' @param which_arms Vector of names of arms that should be run. Defaults to all
#'   arms that have not yet been equipped..
#'
#' @param ... To pass additional parameters as needed
#'
#' @examples
#' library(magrittr)
#' library(parsnip)
#' lm_mod <- linear_reg() %>% set_engine("lm")
#'
#' om <-
#'   octomod() %>%
#'   core(iris) %>%
#'   arm(
#'     title = "t_test",
#'     plan = Sepal.Length + Sepal.Width ~ Petal.Length,
#'     pattern = "direct",
#'     approach = "t.test",
#'     paired = TRUE
#'   ) %>%
#'   arm(
#'     title = "linear",
#'     plan = Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length,
#'     pattern = "sequential",
#'     approach = lm_mod
#'   ) %>%
#'   equip()
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate filter rowwise ungroup
#' @importFrom purrr map
#' @export
#' @name equip
equip <- function(octomod, which_arms = NULL, ...) {

	# Check if its octomod in pipeline
	if (!inherits(octomod, "octomod")) {
		stop("The argument must inherit from the `octomod` class.")
	}

	# Check if core data is present
	if ("list" %in% class(octomod$core)) {
		stop("Cannot equip the `octomod` without `core` data.", call. = FALSE)
	}

	# Check arms
	if (length(octomod$arms) == 0) {
		stop("There are no arms to equip at this time.")
	}

	# Which arms to bear
	bear <- bear_arms(octomod, which_arms)

	# Get core data, which may need to be split by arm
	core <- octomod$core

	# Appropriate fit based on type, tibble to make it easier to split
	# Group and fit by test type
	arms <-
		octomod$arms[bear] %>%
		dplyr::bind_rows(.id = "arm")

	# Split models
	split_models <- filter(arms, type == "model_spec" & !is.na(split))
	if (nrow(split_models) > 0) {
		split_models <-
			split_models %>%
			rowwise() %>%
			mutate(fit = list(fit(approach, formulas, data = filter(core, eval(as.name(split)) == split_level)))) %>%
			ungroup()
	}

	# Unsplit models
	unsplit_models <- filter(arms, type == "model_spec" & is.na(split))
	if (nrow(unsplit_models) > 0) {
		unsplit_models <-
			unsplit_models %>%
			rowwise() %>%
			mutate(fit = list(fit(approach, formulas, data = core))) %>%
			ungroup()
	}

	# Split tests
	split_tests <- arms %>% filter(type == "htest" & !is.na(split))
	if (nrow(split_tests) > 0) {
		split_tests <-
			split_tests %>%
			rowwise() %>%
			mutate(fit = list({
				df <- filter(core, eval(as.name(split)) == split_level)
				y <- df[[outcomes]]
				x <- df[[vars]]
				approach(x, y, pars)
			})) %>%
			ungroup()
	}

	# Unsplit tests
	unsplit_tests <- arms %>% filter(type == "htest" & is.na(split))
	if (nrow(unsplit_tests) > 0) {
		unsplit_tests <-
			unsplit_tests %>%
			rowwise() %>%
			mutate(fit = list({
				y <- core[[outcomes]]
				x <- core[[vars]]
				approach(x, y, pars)
			})) %>%
			ungroup()
	}

	# Get the tidied versions
	equip <-
		dplyr::bind_rows(
			split_models,
			unsplit_models,
			split_tests,
			unsplit_tests
		) %>%
		split(.$arm) %>%
		map(., ~ dplyr::select(.x, c(outcomes, vars, test_num, fit))) %>%
		map(., ~ mutate(.x, tidied = map(
			fit, ~ broom::tidy(.x, conf.int = TRUE, exponentiate = TRUE)
		)))

	# Add to octomod
	octomod[["equipment"]][bear] <- equip[bear]

	# Return
	new_octomod(octomod)

}
