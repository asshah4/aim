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
#' @importFrom rlang !! sym
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

	# Trim to appropriate arms and inventory
	status <- octomod$inventory[bear]
	arms <- octomod$arms[bear]

	# Equipement initialization
	equipment <- list()


	for (i in 1:length(bear)) {

		if (status[[bear[i]]]$test$type == "model_spec") {

			equipment[[bear[i]]] <-
				arms[[bear[i]]] %>%
				rowwise() %>%
				{
					if (status[[bear[i]]]$strata$split)
						mutate(., fit = list(
							possible_fit(
								status[[bear[i]]]$test$approach,
								formulas,
								#data = filter(core, eval(as.name(status[[bear[i]]]$split$var)) == level)
								data = filter(core, !!sym(status[[bear[i]]]$strata$var) == level)
							)
						))
					else
						mutate(., fit = list(
							possible_fit(status[[bear[i]]]$test$approach, formulas, data = core)
						))
				} %>%
				mutate(tidied = list(broom::tidy(
					fit, conf.int = TRUE, exponentiate = TRUE
				))) %>%
				ungroup()

			octomod$inventory[[bear[i]]]$fit$equipped <- TRUE

		}

		if (status[[bear[i]]]$test$type == "htest") {

			equipment[[bear[i]]] <-
				arms[[bear[i]]] %>%
				rowwise() %>%
				{
					if (status[[bear[i]]]$strata$split)
						mutate(., fit = list({
							df <- filter(core, !!sym(status[[bear[i]]]$strata$var) == level)
							y <- df[[outcomes]]
							x <- df[[vars]]
							status[[bear[i]]]$test$approach(x, y, status[[bear[i]]]$test$args)
						}))
					else
						mutate(., fit = list({
							y <- core[[outcomes]]
							x <- core[[vars]]
							status[[bear[i]]]$test$approach(x, y, status[[bear[i]]]$test$args)
						}))
				} %>%
				mutate(tidied = list(broom::tidy(
					fit, conf.int = TRUE, exponentiate = TRUE
				))) %>%
				ungroup()

			octomod$inventory[[bear[i]]]$fit$equipped <- TRUE

		}

	}

	# Add to octomod
	octomod[["equipment"]][bear] <- equipment[bear]

	# Return
	new_octomod(octomod)

}
