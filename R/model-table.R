# Class ------------------------------------------------------------------------

#' Model Tables
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' This function introduces a super class that combines both the `list` class
#' (and its derivative `list_of`) and regression models and/or hypothesis tests.
#' Models that are similar and share certain properties can be combined together
#' into a `mdl_tbl`.
#'
#' @name mdl_tbl
#' @inheritParams rlang::args_dots_empty
#' @export
mdl_tbl <- function(...) {

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
			mtl[[i]] <- construct_table_from_models(dots[[i]])
		}
		if (inherits(dots[[i]], "fmls")) {
			mtl[[i]] <- construct_table_from_formulas(dots[[i]])
		}
	}

	# Return new class
	mdTab <- do.call(vec_rbind, mtl)
	mdTab
}

#' @rdname mdl_tbl
#' @export
model_table <- mdl_tbl

#' @rdname mdl_tbl
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
		class = "mdl_tbl"
	)
}


#' @rdname mdl_tbl
#' @export
is_model_table <- function(x) {
	inherits(x, "mdl_tbl")
}

#' @keywords internal
#' @noRd
methods::setOldClass(c("mdl_tbl", "vctrs_vctr"))

#' @export
print.mdl_tbl <- function(x, ...) {
	cat(sprintf("<%s>\n", class(x)[[1]]))
	cli::cat_line(format(x)[-1])
}

#' @export
vec_ptype_full.mdl_tbl <- function(x, ...) {
	"model_table"
}

#' @export
vec_ptype_abbr.mdl_tbl <- function(x, ...) {
	"mdl_tbl"
}

# Constructors ----


#' Restructure models to fit within a model table
#' Passes information to `new_model_table()` for initialization
#' @param x Vector of `mdl` objects
#' @keywords internal
#' @export
construct_table_from_models <- function(x, ...) {

	# Meta components of the models
	n <- length(x)
	rid <- sapply(x, rlang::hash)
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

	out <-
		sapply(tl, function(.x) {
			.y <- as.character(dplyr::filter(.x, role == "outcome"))
			if (length(.y) == 0) {
				.y <- NA_character_
			} else {
				.y
			}
		}) |>
		as.character()
	exp <-
		sapply(tl, function(.x) {
			.y <- as.character(dplyr::filter(.x, role == "exposure"))
			if (length(.y) == 0) {
				.y <- NA_character_
			} else {
				.y
			}
		}) |>
		as.character()
	med <-
		sapply(tl, function(.x) {
			.y <- as.character(dplyr::filter(.x, role == "mediator"))
			if (length(.y) == 0) {
				.y <- NA_character_
			} else {
				.y
			}
		}) |>
		as.character()
	int <-
		sapply(tl, function(.x) {
			.y <- as.character(dplyr::filter(.x, role == "interaction"))
			if (length(.y) == 0) {
				.y <- NA_character_
			} else {
				.y
			}
		}) |>
		as.character()

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
		id = rid,
		formula_index = fid,
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
construct_table_from_formulas <- function(x, ...) {

	# Meta components of the models
	n <- nrow(x)
	rid <- apply(x, MARGIN = 1, rlang::hash) # Since formulas are matrices
	fits <- rep(FALSE, n)

	# Get terms and formulas
	mf <- x
	tl <- formulas_to_terms(mf)
	fl <- as.character(stats::formula(mf))

	# Formula IDs require checking against the formula matrix
	fid <- apply(mf, MARGIN = 1, list) |> unlist(recursive = FALSE)
	fmMat <- vec_data(mf)

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

	# Data will be empty because this is just a formula
	# Strata may be present, but levels cannot be
	sta <- sapply(tl, function(.x) {
		.y <- as.character(filter(.x, role == "strata"))
		if (length(.y) == 0) {
			.y <- NA_character_
		} else {
			.y
		}
	})



	# Initialize a new list
	res <- df_list(
		id = rid,
		formula_index = fid,
		data_id = NA,
		model_call = NA,
		formula_call = fl,
		outcome = out,
		exposure = exp,
		mediator = med,
		interaction = int,
		strata = sta,
		level = NA,
		model_parameters = NA,
		model_summary = NA,
		fit_status = fits
	)

	# Return
	new_model_table(res,
									formulaMatrix = fmMat,
									termTable = tmTab)
}

#' @importFrom dplyr dplyr_reconstruct
#' @export
dplyr_reconstruct.mdl_tbl <- function(data, template) { # nolint
	model_table_reconstruct(x = data, to = template)
}

#' @importFrom dplyr dplyr_row_slice
#' @export
dplyr_row_slice.mdl_tbl <- function(data, i, ...) {
	model_table_reconstruct(vec_slice(data, i), data)
}

#' @export
model_table_reconstruct <- function(x, to) {
	if (model_table_reconstructable(x, to)) {
		df_reconstruct(x, to)
	} else {
		# Return without reconstructing...
		x <- as.data.frame(x)
		message("Removing invariant columns in `<mdl_tbl>` returns `<data.frame>` object")
		x
	}

}

#' If objects are model tables, attributes are carried over to subset object
#' @keywords internal
#' @noRd
df_reconstruct <- function(x, to) {

	validate_model_table(to)
	tmTab <- attr(to, "termTable")
	fmMat <- attr(to, "formulaMatrix")

	# Used hash-ids to get the right formulas
	newMat <-
		fmMat[which(to$id %in% x$id), ] |>
		{
			\(.x) .x[colSums(.x) > 0]
		}()

	# Special terms and the rest of terms in formulas
	out <- unique(to$outcome)
	exp <- unique(to$exposure)
	med <- unique(to$mediator)
	int <- unique(to$interaction)
	sta <- unique(to$strata)
	special <- na.omit(c(out, exp, med, int, sta))
	others <- setdiff(names(newMat), special)

	newTab <- dplyr::bind_rows(
		filter(tmTab, term %in% out & role == "outcome"),
		filter(tmTab, term %in% exp & role == "exposure"),
		filter(tmTab, term %in% med & role == "mediator"),
		filter(tmTab, term %in% int & role == "interaction"),
		filter(tmTab, term %in% sta & role == "strata"),
		filter(tmTab, term %in% others)
	)

	# Update attributes of template/target
	attr(to, "termTable") <- newTab
	attr(to, "formulaMatrix") <- newMat

	# Incorporate new attributes and return
	attrs <- attributes(to)
	attrs$names <- names(x)
	attrs$row.names <- .row_names_info(x, type = 0L)
	attributes(x) <- attrs

	# Return the original table (= x) with updated attributes
	x

}

#' Can `mdl_tbl` be reconstructed based on invariants?
#' @keywords internal
#' @param x data frame that will have invariants checked
#' @param to the tibble subclass of `mdl_tbl` that would be reconstructed
#' @export
model_table_reconstructable <- function(x, to) {

	# Invariant columns must be present
	XCols <- names(x)
	ToCols <- names(to)
	if (all(is.element(XCols, ToCols))) {
		return(TRUE)
	} else {
		return(FALSE)
	}

}

#' Model table object validation
#' @keywords internal
#' @param x data frame that will have invariants checked
#' @export
validate_model_table <- function(x) {

	# Invariant columns must be present
	cols <- names(x)

	modelCols <-
		c("model_call",
			"formula_call",
			"outcome",
			"model_parameters",
			"model_summary")

	formulaCols <-
		c("formula_call", "outcome")

	if (all(modelCols %in% cols)) {
		colStatus <- TRUE
	} else if (all(formulaCols %in% cols)) {
		colStatus <- TRUE
	} else {
		colStatus <- FALSE
	}

	stopifnot(
		"Model table does not contain invariant columns" = isTRUE(colStatus)
	)

	invisible(x)

}

# Casting and coercion ---------------------------------------------------------

# SELF

#' @export
mdl_tbl_ptype2 <- function(x, y, ..., x_arg = "", y_arg = "") {

  # Create a temporary/new structure of the table
	mdTab <- tib_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)

	# Terms must be coalesced together
	xTm <- attr(x, "termTable")
	yTm <- attr(y, "termTable")
	tmTab <-
		rbind(xTm, yTm) |>
		unique()

	# Formula matrices must bound together
	xFm <- attr(x, "formulaMatrix")
	yFm <- attr(y, "formulaMatrix")
  fmMat <-
    dplyr::bind_rows(xFm, yFm) |>
	  {
	    \(.x) replace(.x, is.na(.x), 0)
	  }()

  # Output with correct scalar attributes
	new_model_table(x = as.list(mdTab),
	                formulaMatrix = fmMat,
	                termTable = tmTab)

}

#' @export
mdl_tbl_cast <- function(x, to, ..., x_arg = "", to_arg = "") {

	# Terms must be coalesced together
	toTm <- attr(to, "termTable")
	xTm <- attr(x, "termTable")
	tmTab <-
		rbind(toTm, xTm) |>
		unique()

	# Formula matrices must bound together
	toFm <- attr(to, "formulaMatrix")
	xFm <- attr(x, "formulaMatrix")
  fmMat <-
    dplyr::bind_rows(toFm, xFm) |>
	  {
	     \(.x) replace(.x, is.na(.x), 0)
	  }()

  # Create a temporary/new structure of the table
  mdTab <- tib_cast(x, to, ..., x_arg = x_arg, y_arg = y_arg)

  # Output with correct scalar attributes
	new_model_table(x = as.list(mdTab),
	                formulaMatrix = fmMat,
	                termTable = tmTab)

}

#' @export
vec_ptype2.mdl_tbl.mdl_tbl <- function(x, y, ...) {
	mdl_tbl_ptype2(x, y, ...)
}

#' @export
vec_cast.mdl_tbl.mdl_tbl <- function(x, to, ...) {
	mdl_tbl_cast(x, to, ...)
}
