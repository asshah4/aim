#' Validate `hypothesis` object
#' @noRd
validate_hypothesis <- function(h) {

	# Class
	validate_class(h, "hypothesis")

	# Data
	validate_class(attr(h, "data"), c("tbl", "tbl_df", "data.frame"))

	# Combination
	validate_class(attr(h, "combination"), "character")

	# Check optional parameters

	# Return check
	invisible(TRUE)

}

#' Validation of class inheritance
#' @noRd
validate_class <- function(x, what) {
	if (!inherits(x, what)) {
		stop(
			deparse(substitute(x)),
			" needs to inherit from `",
			paste("c(", paste(what, collapse = ", ") , ")", sep = ""),
			"`, but is of class `",
			class(x),
			"`.",
			call. = FALSE
		)
	}

	# Return
	invisible(TRUE)

}

#' Validate that study is at the appropriate stage for subsequent pipe functions
#' @noRd
validate_stage <- function(x, stage) {

	# Get status table
	y <- attr(x, "status_table")

	# Validate that the study contains hypothesis objects
	switch(
		stage,
		hypothesis = {
			if (nrow(y) == 0) {
				stop(
					deparse(substitute(x)),
					" does not yet have `hypothesis` objects added. Please call `add_hypothesis()` prior to construction.",
					call. = FALSE
				)
			}
		},
		run = {
			if (!any(y$run)) {
				stop(
					deparse(substitute(x)),
					" does not have any `hypothesis` objects that have been run. Please call `construct_tests()` first.",
					call. = FALSE
				)
			}
		},
		extract = {
			if (!any(y$tidy)) {
				stop(
					deparse(substitute(x)),
					" does not have any `hypothesis` objects that have been tidied or summarized. Please call `extract_results()` first.",
					call. = FALSE
				)
			}

		}
	)

	# Return
	invisible(TRUE)

}

#' Check if hypothesis in study has been run or not
#' @noRd
check_hypothesis <- function(x, name, run = TRUE) {

	# Get status table
	y <- attr(x, "status_table")
	if (run %in% y$run[y$name == name]) {
		# Hypothesis has requested status
		invisible(TRUE)
	} else {
		stop("Hypothesis `",
				 name,
				 "` should have run status of `",
				 run,
				 "` for this test."
		)
	}

}

#' Check if formula terms are in data set
#' @noRd
check_terms <- function(x, data) {

	# Strip away unnecessary elements from formula to get pure terms
	vars <- all.vars(x)
	cols <- names(data)

	if (!all(vars %in% cols)) {
		y <- vars[which(!(vars %in% cols))]
		stop(
			"The terms c(",
			paste0("'", y, "'", collapse = ", "),
			") are not contained in the data set `",
			deparse(substitute(data)),
			"`."
		)
	}

	invisible(TRUE)

}

#' Check if labels are describing formula objects
#' @noRd
check_labels <- function(x, labels) {

	if (!is.null(labels)) {
		validate_class(labels, "list")
	}

	rx::is.named(labels)
	formula_vars <- all.vars(x)
	label_vars <- unlist(unname(labels))
	label_names <- names(labels)

	if (!all(label_vars %in% formula_vars)) {
		y <- label_vars[which(!(label_vars %in% formula_vars))]
		stop(
			"The terms c(",
			paste0("'", y, "'", collapse = ", "),
			") are not contained in the formula."
		)
	}

	invisible(TRUE)

}

# Model Constructs -------------------------------------------------------------

#' Check of models can be combined into a suit, returns a compatability table
#' @noRd
validate_model_compatability <- function(x) {

	# Should be a list of model_archetypes
	model_list <- list()

	for (i in seq_along(x)) {

		# Validate
		validate_class(x[[i]], "model_archetype")

		# Terms
		t <-
			attr(x[[i]], "terms") |>
			vec_data() |>
			subset(select = c(term, side, role))

		# Model information
		m <-
			vec_data(x[[i]]) |>
			subset(select = c(tag, type, subtype))

		# Combine into list of data.frames
		# Suppress warning about dropping row names
		model_list[[i]] <-
			cbind(m, t) |>
			suppressWarnings()
	}

	# Make a unique table
	tbl <-
		do.call(rbind, model_list) |>
		unique()

	# Validate model types
	if ( (length(unique(tbl$type)) != 1) | (length(unique(tbl$subtype)) != 1)) {
		stop(
			"The models need to have the same type [",
			paste(unique(tbl$type), collapse = ", "),
			"] and subtype [",
			paste(unique(tbl$subtype), collapse = ", "),
			"]",
			call. = FALSE
		)
	}

	# Validate outcome and exposure
	out <- unique(tbl$term[tbl$role == "outcome"])
	prd <- unique(tbl$term[tbl$role == "predictor"])
	exp <- unique(tbl$term[tbl$role == "exposure"])
	cov <- unique(tbl$term[tbl$role == "covariate"])
	med <- unique(tbl$term[tbl$role == "mediator"])
	unk <- unique(tbl$term[tbl$role == "unknown"])

	if (length(out) > 1 & length(exp) > 1) {
		stop(
			"If there are multiple outcomes [",
			paste(out, collapse = ", "),
			"] and multiple exposures [",
			paste(exp, collapse = ", "),
			"], the models are not related enough to group together",
			call. = FALSE
		)
	}

	# Else return invisibly true
	invisible(TRUE)

}
