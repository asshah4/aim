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
#' @importFrom dplyr mutate
#' @export
md_tbl <- function(...) {

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

	stopifnot("Only the <mdl> class is currently supported" =
							inherits(dots, "mdl"))

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

new_model_table <- function(x = data.frame(),
														formulaMatrix = data.frame(),
														termTable = data.frame()) {

	new_tibble(
		x,
		formulaMatrix = formulaMatrix,
		termTable = termTable,
		class = "md_tbl",
	)
}

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

#' @keywords internal
#' @noRd




