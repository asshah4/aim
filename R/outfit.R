#' Outfit the `octomod` With Hypothesis Tests
#'
#' This function allows for fitting the many arms of the `octomod` object,
#' outfitting them with the correct test methods. For modeling, this heavily
#' leans on the `tidymodels` approach which allows for unified model
#' specification. For inferential statistics, which do not yet have a unified
#' testing approach, this provides the ability to run these somewhat simpler
#' tests in a cohesive manner.
#'
#' @return An `octomod` object with arms outfitted with the test statistics.
#'
#' @param octomod Object of class `octomod`
#'
#' @param which_arms Vector of names of arms that should be run. Defaults to all
#'   arms that have not yet been outfitted.
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
#'   add_core(iris) %>%
#'   add_arm(
#'     title = "t_test",
#'     f = Sepal.Length + Sepal.Width ~ Petal.Length,
#'     pattern = "direct",
#'     approach = "t.test",
#'     paired = TRUE
#'   ) %>%
#'   add_arm(
#'     title = "linear",
#'     f = Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length,
#'     pattern = "sequential",
#'     approach = lm_mod
#'   ) %>%
#'   add_outfit()
#'
#' @importFrom magrittr %>%
#' @export
#' @name outfit
add_outfit <- function(octomod, which_arms = NULL, ...) {

	# Check if its octomod in pipeline
	if (!inherits(octomod, "octomod")) {
		stop("The argument must inherit from the `octomod` class.")
	}

	# Check if core data is present
	if ("list" %in% class(octomod$core)) {
		stop("Cannot outfit the `octomod` without `core` data.", call. = FALSE)
	}

	# Check arms
	if (length(octomod$arms) == 0) {
		stop("There are no arms to outfit at this time.")
	}

	# Which arms to bear
	# Are the arms given contained within the octomod?
	# If so, capture the arms beared
	if (is.null(which_arms)) {
		bear <- names(octomod$arms)
	} else {
		if (!which_arms %in% names(octomod$arms)) {
			stop("The named arms are not available in the `octomod`", call. = FALSE)
		} else {
			bear <- which_arms
		}
	}

	# Get core data
	core <- octomod$core

	# Appropriate fit based on type, tibble to make it easier to split
	# Group and fit by test type
	arms <-
		octomod$arms[bear] %>%
		dplyr::bind_rows(.id = "arm")

	# Parsnip models, the bulk of the octomod
	parsnips <-
		arms %>%
		dplyr::filter(type == "model_spec") %>%
		dplyr::mutate(fit = purrr::pmap(
			list(approach, formulas),
			function(approach, formulas) {
				fit(approach, formulas, data = core)
			}
		))

	# Hypothesis testing for individual models
	tests <-
		arms %>%
		dplyr::filter(type == "htest") %>%
		dplyr::mutate(fit = purrr::pmap(
			list(outcomes, vars, pars, approach),
			function(y, x, p, fn) {
				y <- core[[y]]
				x <- core[[x]]
				fn(x, y, p)
			}
		))

	# Format for output
	outfit <-
		dplyr::bind_rows(parsnips, tests) %>%
		split(.$arm) %>%
		purrr::map(., ~ dplyr::select(.x, c(outcomes, test_num, fit))) %>%
		purrr::map(., ~ dplyr::mutate(.x, tidied = purrr::map(
			fit, ~ broom::tidy(.x, conf.int = TRUE, exponentiate = TRUE)
		)))

	# Add to octomod
	octomod[["outfit"]][bear] <- outfit[bear]

	# Return
	octomod

}

