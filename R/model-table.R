# Class ------------------------------------------------------------------------

#' Model Tables
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' This function introduces a super class that combines both the `list` class
#' (and its derivative `list_of`) and regression models and/or hypothesis tests.
#' Models that are similar and share certain properties can be combined together
#' into a `md_tbl`.
#'
#' @name md_tbl
#' @inheritParams rlang::args_dots_empty
#' @importFrom dplyr mutate
#' @export
md_tbl <- function(...) {

	rlang::check_dots_empty()
	# Steps:
	# 	Assess model...
	# 		Model fit/information
	# 		Parameter estimates
	#			Formula/terms
	#		Re-organize information into...
	#			Model data frame
	#			Formula "matrix"
	#			Terms (+/- labels and other meta info)
	dots <- rlang::list2(...)
	if (length(dots) == 0) {
		return(new_model_table())
	}
	# Handles either a list or a series of arguments
	if (length(dots) == 1 && is.list(dots[[1]])) {
		dots <- dots[[1]]
	}

	# Model Table Lists...
	mtl <- vector("list", length(dots))

	for (i in seq_along(dots)) {
		if (inherits(dots[[i]], "mdl")) {
			mtl[[i]] <- construct_model_table(dots[[i]])
		}
	}

	# Return new class
	new_model_table(
		x = mdTab,
		formulatMatrix = fmMat,
		termTable = tmTab
	)
}

#' @rdname md_tbl
#' @export
model_table <- md_tbl


#' @rdname md_tbl
#' @export
is_model_table <- function(x) {
	inherits(x, "md_tbl")
}

#' @keywords internal
#' @noRd
methods::setOldClass(c("md_tbl", "vctrs_vctr"))

#' @export
print.md_tbl <- function(x, ...) {
	cat(sprintf("<%s>\n", class(x)[[1]]))
	cli::cat_line(format(x)[-1])
}

#' @export
vec_ptype_full.md_tbl <- function(x, ...) {
	"model_table"
}

#' @export
vec_ptype_abbr.md_tbl <- function(x, ...) {
	"md_tbl"
}

# Constructors ----

#' Restructure models to fit within a model table
#' @param x Vector of `mdl` objects
#' @keywords internal
#' @export
construct_model_table <- function(x, ...) {

	# Meta components of the models
	n <- length(x)
	mid <- sapply(x, rlang::hash)
	fits <- rep(TRUE, n)

	# Components of the model fields
	mc <- unlist(field(x, "modelCall"))
	ma <- field(x, "modelArgs")
	pe <- field(x, "parameterEstimates")
	si <- field(x, "summaryInfo")

	# Get terms and formulas
	mf <- field(x, "modelFormula")
	tl <- lapply(mf, key_terms)
	fl <- unname(sapply(mf, as.character, USE.NAMES = FALSE))

	# Formula IDs require checking against the formula matrix
	fid <- lapply(mf, unlist, recursive = FALSE)
	fmMat <-
		lapply(mf, vec_proxy) |>
		do.call(what = rbind, args = _) |>
		vec_data()

	# Terms must be combined into a term table for later look up
	tmTab <-
		lapply(tl, vec_data) |>
		do.call(what = rbind, args = _) |>
		unique()

	out <- sapply(tl, function(.x) {
		.y <- as.character(filter(.x, role == "outcome"))
		if (length(.y) == 0) {
			.y <- NA_character_
		} else {
			.y
		}
	})
	exp <- sapply(tl, function(.x) {
		.y <- as.character(filter(.x, role == "exposure"))
		if (length(.y) == 0) {
			.y <- NA_character_
		} else {
			.y
		}
	})
	med <- sapply(tl, function(.x) {
		.y <- as.character(filter(.x, role == "mediator"))
		if (length(.y) == 0) {
			.y <- NA_character_
		} else {
			.y
		}
	})
	int <- sapply(tl, function(.x) {
		.y <- as.character(filter(.x, role == "interaction"))
		if (length(.y) == 0) {
			.y <- NA_character_
		} else {
			.y
		}
	})

	# Get all data names and strata variables back
	da <- field(x, "dataArgs")
	did <- sapply(da, function(.x) {
		.x$dataName
	})
	sta <- sapply(da, function(.x) {
		.x$strataVariable
	})
	slvl <- sapply(da, function(.x) {
		.x$strataLevel
	})

	# Initialize a new list
	res <- df_list(
		model_id = mid,
		formula_id = fid,
		data_id = did,
		model_call = mc,
		formula_call = fl,
		outcome = out,
		exposure = exp,
		mediator = med,
		interaction = int,
		strata = sta,
		level = slvl,
		model_parameters = pe,
		model_summary = si,
		fit_status = fits
	)

	# Return
	new_model_table(res,
									formulaMatrix = fmMat,
									termTable = tmTab)

}

#' Restructure formulas to fit within a model table
#' @param x Vector of `fmls` objects
#' @keywords internal
#' @export
construct_formula_table <- function(x, ...) {

	new_model_table()
}

#' Initialize new model-based tibble / tbl_df
#' @keywords internal
#' @export
new_model_table <- function(x = list(),
														formulaMatrix = data.frame(),
														termTable = data.frame(),
														...) {

	# Invariant rules:
	#		Can add and remove rows (each row is essentially a model)
	#		Rows can be re-ordered
	#		Columns cannot be re-ordered
	#
	#	Invariant columns:
	#		Key Relationship: outcome and exposure, roles, etc
	#		Context: Formula (giving information on covariates) and Model Type
	#		Fit: Individual parameters and model level estimates/statistics

	checkmate::assert_class(formulaMatrix, "data.frame")
	checkmate::assert_class(termTable, "data.frame")
	checkmate::assert_list(
		x,
		types = c("character", "list", "factor", "logical", "numeric"),
		len = 14
	)

	new_tibble(
		x,
		formulaMatrix = formulaMatrix,
		termTable = termTable,
		class = "md_tbl"
	)
}

# Casting and coercion ---------------------------------------------------------

#' @export
md_tbl_ptype2 <- function(x, y, ..., x_arg = "", y_arg = "") {
	out <- tib_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)


	new_model_table(out)
}

#' @export
md_tbl_cast <- function(x, to, ..., x_arg = "", to_arg = "") {
	out <- tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)

	new_model_table(out)
}

#' @export
vec_ptype2.md_tbl.md_tbl <- function(x, y, ...) {
	md_tbl_ptype2(x, y, ...)
}

#' @export
vec_cast.md_tbl.md_tbl <- function(x, to, ...) {
	md_tbl_cast(x, to, ...)
}

# TIBBLE

#' @export
vec_ptype2.md_tbl.tbl_df <- function(x, y, ...) {
	md_tbl_ptype2(x, y, ...)
}

#' @export
vec_ptype2.tbl_df.md_tbl <- function(x, y, ...) {
	md_tbl_ptype2(x, y, ...)
}

#' @export
vec_cast.md_tbl.tbl_df <- function(x, to, ...) {
	tib_cast(x, to, ...)
}

#' @export
vec_cast.tbl_df.md_tbl <- function(x, to, ...) {
	tib_cast(x, to, ...)
}

